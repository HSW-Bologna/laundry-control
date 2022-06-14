port module Pages.Machine.Page exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (getTranslation, languageFromString)
import AppData.IpAddress as IpAddress exposing (IpAddress, changePart, localhost, toString)
import AppData.Things5 exposing (Device, deviceDecoder)
import AppData.WashingMachineConfiguration as WMC exposing (MachineConfiguration, MachineParameters, extractArchive)
import AppData.WashingMachineState as WSS
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Array exposing (Array)
import Array.Extra as Array
import Browser
import Bytes exposing (Bytes)
import Bytes.Extra as Bytes
import Context exposing (Context, translate)
import Element as Ui
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FontAwesome.Transforms exposing (RepositionTransform(..))
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Machine.Tabs.MachineConfiguration as ParMacTab
import Pages.Machine.Tabs.RemoteControl as RemControlTab
import Pages.Machine.Tabs.RemoteDevicesList as RemDevListTab
import Pages.Machine.Tabs.WashingCycle as WashCycleTab
import Ports as Ports exposing (decodeEvent, getRemoteMachineConfiguration)
import Task
import Time
import Widget as Widget
import Widget.Material as Material
import Widget.Snackbar as Snackbar



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { context : Context
    , leftDrawerVisible : Bool
    , rightMenuVisible : Bool
    , visibleModal : VisibleModal
    , programListExpanded : Bool
    , tabModel : TabModel
    , config : Maybe MachineConfiguration
    , snackbar : Snackbar.Snackbar String
    , connectionState : WSS.ConnectionState
    , sensorsData : Array WSS.Sensors
    , localMachines : Maybe (List ( IpAddress, String ))
    , things5Token : Maybe String
    , things5Devices : List Device
    }


init : String -> ( Model, Cmd Msg )
init language =
    let
        initialContext =
            Context <| languageFromString language
    in
    ( { context = initialContext
      , leftDrawerVisible = False
      , rightMenuVisible = False
      , visibleModal = NoModal
      , programListExpanded = False
      , tabModel = NoTab
      , config = Nothing
      , snackbar = Snackbar.init
      , connectionState = WSS.Disconnected
      , localMachines = Nothing
      , sensorsData = Array.empty
      , things5Token = Nothing
      , things5Devices = []
      }
    , Cmd.none
    )


type TabModel
    = MachineConfigurationModel ParMacTab.Model
    | RemoteControlModel RemControlTab.Model
    | WashCyclesModel WashCycleTab.Model
    | RemoteDevicesListModel RemDevListTab.Model
    | NoTab


type VisibleModal
    = NoModal
    | IpInput IpAddress



-- UPDATE


toMachineConfigurationTabModel : MachineConfiguration -> TabModel
toMachineConfigurationTabModel config =
    ParMacTab.buildModel config
        |> MachineConfigurationModel


toWashingCycleTabModel : Int -> WMC.WashingCycle -> WMC.MachineParameters -> TabModel
toWashingCycleTabModel index cycle parmac =
    WashCycleTab.buildModel index cycle parmac
        |> WashCyclesModel


fromMachineConfigurationTabModel : ParMacTab.Model -> Model -> Model
fromMachineConfigurationTabModel tabModel model =
    { model | config = Just tabModel.config, tabModel = MachineConfigurationModel tabModel }


fromRemoteDevicesListTabModel : ( Model, RemDevListTab.Model, Cmd msg ) -> ( Model, Cmd msg )
fromRemoteDevicesListTabModel ( model, tabModel, cmd ) =
    ( { model | tabModel = RemoteDevicesListModel tabModel }, cmd )


fromRemoteControlTabModel : ( Model, RemControlTab.Model, Cmd msg ) -> ( Model, Cmd msg )
fromRemoteControlTabModel ( model, tabModel, cmd ) =
    ( { model | tabModel = RemoteControlModel tabModel }, cmd )


fromWashingCycleTabModel : WashCycleTab.Model -> Model -> Model
fromWashingCycleTabModel tabModel model =
    let
        newConfig =
            Maybe.map (\c -> { c | cycles = Array.update tabModel.index (always tabModel.cycle) c.cycles }) model.config
    in
    { model | tabModel = WashCyclesModel tabModel, config = newConfig }


toRemoteControl : TabModel
toRemoteControl =
    RemoteControlModel RemControlTab.buildModel


toRemoteDevicesList : TabModel
toRemoteDevicesList =
    RemoteDevicesListModel RemDevListTab.buildModel


type Msg
    = SaveMachineConfig
    | LoadMachineFromLocal
    | CreateNewMachineConfig
    | MachineSelected File
    | MachineLoaded Bytes
    | StupidElmMachineLoaded (Array Int)
    | LeftDrawerToggle
    | RightMenuToggle
    | ProgramListToggle Bool
    | MachineConfigurationMsg ParMacTab.Msg
    | RemoteControlMsg RemControlTab.Msg
    | RemoteDevicesListMsg RemDevListTab.Msg
    | WashCyclesMsg WashCycleTab.Msg
    | ChangeTab TabModel
    | ChangeTabCycle Int
    | TimePassed Int
    | Things5Token String
    | Things5Devices Encode.Value
    | StateUpdate Encode.Value
    | IpAddresses Encode.Value
    | InsertIpAddress
    | IpAddessChange IpAddress
    | LocalConnectionRequest (Maybe IpAddress)
    | BackendSnackbarMessage String
    | RefreshDiscovery


port ipAddresses : (Encode.Value -> msg) -> Sub msg


port stateUpdate : (Encode.Value -> msg) -> Sub msg


port things5Login : (String -> msg) -> Sub msg


port things5Devices : (Encode.Value -> msg) -> Sub msg


port remoteMachineLoaded : (Array Int -> msg) -> Sub msg


port notificationMessage : (String -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        closeTab m =
            { m | tabModel = NoTab }

        newRawMessage s m =
            { m | snackbar = Snackbar.dismiss model.snackbar |> Snackbar.insert s }

        newMessage s m =
            newRawMessage (translate s m.context) m

        hideMenu m =
            { m | rightMenuVisible = False }

        hideModal m =
            { m | visibleModal = NoModal }

        fillTabWithConfig config m =
            let
                updatedModel =
                    { m | config = Just config, tabModel = toMachineConfigurationTabModel config }
            in
            case m.tabModel of
                MachineConfigurationModel _ ->
                    updatedModel

                NoTab ->
                    updatedModel

                _ ->
                    { m | config = Just config }

        addSensorsData connection m =
            case connection of
                WSS.Connected { sensors } _ _ ->
                    let
                        newUnboundedData =
                            Array.push sensors m.sensorsData

                        newData =
                            if Array.length newUnboundedData > 60 then
                                Array.removeAt 0 newUnboundedData

                            else
                                newUnboundedData
                    in
                    { m | sensorsData = newData }

                _ ->
                    m

        fillTabWithConnection connection m =
            case m.tabModel of
                NoTab ->
                    { m | connectionState = connection, tabModel = toRemoteControl }

                _ ->
                    { m | connectionState = connection }
    in
    case ( msg, model.tabModel ) of
        -- Global messages
        ( TimePassed int, _ ) ->
            ( { model | snackbar = model.snackbar |> Snackbar.timePassed int }, Cmd.none )

        ( IpAddresses value, _ ) ->
            ( decodeEvent
                (Decode.list
                    (Decode.andThen
                        (\l ->
                            case l of
                                ip :: node :: _ ->
                                    Maybe.map
                                        (\validIp ->
                                            Decode.succeed ( validIp, node )
                                        )
                                        (IpAddress.fromString ip)
                                        |> Maybe.withDefault (Decode.fail "Invalid ip list")

                                _ ->
                                    Decode.fail "Invalid ip list"
                        )
                        (Decode.list Decode.string)
                    )
                )
                value
                |> Result.toMaybe
                |> Maybe.map (\ips -> { model | localMachines = Just ips })
                |> Maybe.withDefault model
            , Cmd.none
            )

        ( Things5Token token, _ ) ->
            ( { model | things5Token = Just token }, Cmd.none )

        ( Things5Devices value, _ ) ->
            ( case Decode.decodeValue (Decode.list deviceDecoder) value of
                Ok devices ->
                    { model | things5Devices = devices }

                _ ->
                    model
            , Cmd.none
            )

        ( StateUpdate state, _ ) ->
            case decodeEvent WSS.connectionStateUpdateDecoder state of
                Ok res ->
                    ( model |> fillTabWithConnection res |> addSensorsData res, Cmd.none )

                Err error ->
                    ( newRawMessage (Decode.errorToString error) model, Cmd.none )

        ( BackendSnackbarMessage string, _ ) ->
            ( Intl.codeFromString string
                |> Maybe.map (\message -> newMessage message model)
                |> Maybe.withDefault model
            , Cmd.none
            )

        ( InsertIpAddress, _ ) ->
            ( { model | visibleModal = IpInput localhost }, Ports.searchMachines )

        ( RefreshDiscovery, _ ) ->
            ( { model | localMachines = Nothing }, Ports.searchMachines )

        ( IpAddessChange ip, _ ) ->
            ( { model | visibleModal = IpInput ip }, Cmd.none )

        ( LocalConnectionRequest (Just ip), _ ) ->
            ( model |> hideModal |> hideMenu, Ports.washingMachineHttpConnect (toString ip) )

        ( LocalConnectionRequest Nothing, _ ) ->
            ( model |> hideModal |> hideMenu, Cmd.none )

        ( CreateNewMachineConfig, _ ) ->
            ( model
                |> newMessage Intl.NuovaConfigurazioneCreata
                |> hideMenu
                |> fillTabWithConfig (WMC.default model.context)
            , Cmd.none
            )

        ( SaveMachineConfig, _ ) ->
            ( model
                |> newMessage Intl.ConfigurazioneSalvata
                |> hideMenu
            , Maybe.map (\c -> Download.bytes (c.parmac.name ++ ".WS2020.tar.gz") "application/gzip" <| WMC.createArchive c) model.config
                |> Maybe.withDefault Cmd.none
            )

        ( LoadMachineFromLocal, _ ) ->
            ( model, Select.file [ "application/gzip" ] MachineSelected )

        ( MachineSelected file, _ ) ->
            ( model, Task.perform MachineLoaded <| File.toBytes file )

        ( StupidElmMachineLoaded stupidElmdata, _ ) ->
            ( (case extractArchive (Bytes.fromByteValues <| Array.toList stupidElmdata) of
                Just config ->
                    model
                        |> newMessage Intl.ConfigurazioneCaricata
                        |> fillTabWithConfig config

                Nothing ->
                    newMessage Intl.ConfigurazioneNonValida model
              )
                |> hideMenu
            , Cmd.none
            )

        ( MachineLoaded data, _ ) ->
            ( (case extractArchive data of
                Just config ->
                    model
                        |> newMessage Intl.ConfigurazioneCaricata
                        |> fillTabWithConfig config

                Nothing ->
                    newMessage Intl.ConfigurazioneNonValida model
              )
                |> hideMenu
            , Cmd.none
            )

        ( LeftDrawerToggle, _ ) ->
            ( { model | leftDrawerVisible = not model.leftDrawerVisible } |> hideMenu, Cmd.none )

        ( ProgramListToggle toggle, _ ) ->
            ( case model.config of
                Just _ ->
                    { model | programListExpanded = toggle } |> hideMenu

                Nothing ->
                    { model | programListExpanded = False }
            , Cmd.none
            )

        ( RightMenuToggle, _ ) ->
            ( { model | rightMenuVisible = not model.rightMenuVisible }, Cmd.none )

        ( ChangeTab change, _ ) ->
            ( { model | tabModel = change } |> hideMenu, Cmd.none )

        ( ChangeTabCycle index, _ ) ->
            case model.config of
                Just config ->
                    let
                        newCycles =
                            if index == Array.length config.cycles then
                                Array.push (WMC.newWashingCycle index) config.cycles

                            else
                                config.cycles

                        newConfig =
                            { config | cycles = newCycles }

                        cycle =
                            Array.get index newCycles
                    in
                    ( Maybe.map
                        (\unwrappedCycle ->
                            { model | config = Just newConfig, tabModel = toWashingCycleTabModel index unwrappedCycle newConfig.parmac }
                                |> hideMenu
                        )
                        cycle
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        -- Machine Configuration tab messages
        ( MachineConfigurationMsg tabMsg, MachineConfigurationModel tabModel ) ->
            ( fromMachineConfigurationTabModel (ParMacTab.update tabMsg model tabModel) model
            , Cmd.none
            )

        -- Remote device list tab messages
        ( RemoteDevicesListMsg tabMsg, RemoteDevicesListModel tabModel ) ->
            fromRemoteDevicesListTabModel <| RemDevListTab.update tabMsg model tabModel

        -- Remote Control tab messages
        ( RemoteControlMsg tabMsg, RemoteControlModel tabModel ) ->
            fromRemoteControlTabModel <| RemControlTab.update tabMsg model tabModel

        -- Wash Cycle parameters tab messages
        ( WashCyclesMsg tabMsg, WashCyclesModel tabModel ) ->
            let
                modelSwappedCycles newTabModel oldIndex newIndex =
                    Maybe.map
                        (\realConfig ->
                            let
                                ( newConfig, effectiveNewIndex ) =
                                    WMC.swapWashCycles oldIndex newIndex realConfig
                            in
                            fromWashingCycleTabModel { newTabModel | index = effectiveNewIndex } { model | config = Just newConfig }
                        )
                        model.config
                        |> Maybe.withDefault model
            in
            ( case WashCycleTab.update tabMsg model tabModel of
                ( newTabModel, WashCycleTab.MoveUp index ) ->
                    modelSwappedCycles newTabModel index (index - 1)

                ( newTabModel, WashCycleTab.MoveDown index ) ->
                    modelSwappedCycles newTabModel index (index + 1)

                ( newTabModel, WashCycleTab.Copy cycle index ) ->
                    let
                        newConfig =
                            Maybe.map (\c -> { c | cycles = Array.insertAt (index + 1) cycle c.cycles }) model.config
                    in
                    fromWashingCycleTabModel newTabModel { model | config = newConfig }

                ( newTabModel, WashCycleTab.Remove index ) ->
                    let
                        newConfig =
                            Maybe.map (\c -> { c | cycles = Array.removeAt index c.cycles }) model.config
                    in
                    fromWashingCycleTabModel newTabModel { model | config = newConfig } |> closeTab

                ( newTabModel, WashCycleTab.None ) ->
                    fromWashingCycleTabModel newTabModel model
            , Cmd.none
            )

        -- Runtime silent boilerplate-related errors
        ( _, _ ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 100 (always (TimePassed 100))
        , stateUpdate StateUpdate
        , things5Login Things5Token
        , things5Devices Things5Devices
        , ipAddresses IpAddresses
        , remoteMachineLoaded StupidElmMachineLoaded
        , notificationMessage BackendSnackbarMessage
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        selectedTab =
            case model.tabModel of
                RemoteDevicesListModel _ ->
                    Just 0

                RemoteControlModel _ ->
                    Just 1

                MachineConfigurationModel _ ->
                    Just 2

                WashCyclesModel _ ->
                    Nothing

                NoTab ->
                    Nothing

        selectedCycle =
            case model.tabModel of
                WashCyclesModel { index } ->
                    Just index

                _ ->
                    Nothing

        tabRouter =
            case model.tabModel of
                MachineConfigurationModel tabModel ->
                    ParMacTab.view model tabModel |> Ui.map MachineConfigurationMsg

                RemoteDevicesListModel tabModel ->
                    RemDevListTab.view model tabModel |> Ui.map RemoteDevicesListMsg

                RemoteControlModel tabModel ->
                    RemControlTab.view model tabModel |> Ui.map RemoteControlMsg

                WashCyclesModel tabModel ->
                    WashCycleTab.view model tabModel |> Ui.map WashCyclesMsg

                NoTab ->
                    Ui.el [ Ui.centerX, Ui.centerY ] menuOptions

        menuOptions =
            AppWidgets.rightMenu model.context
                [ ( Intl.CaricaConfigurazione, Just LoadMachineFromLocal )
                , ( Intl.SalvaConfigurazione, Maybe.map (always SaveMachineConfig) model.config )
                , ( Intl.NuovaConfigurazione, Just CreateNewMachineConfig )
                , ( Intl.ConnessioneLocale, Just InsertIpAddress )
                ]

        rightMenuAddition =
            if model.rightMenuVisible then
                [ Ui.below menuOptions ]

            else
                []

        modalAddition =
            case model.visibleModal of
                NoModal ->
                    []

                IpInput ip ->
                    AppWidgets.ipDialog model.context model.localMachines ip IpAddessChange LocalConnectionRequest RefreshDiscovery

        cycles =
            Maybe.map
                (\config ->
                    Array.map
                        (\c -> getTranslation model.context.language c.name)
                        config.cycles
                        |> Array.toList
                )
                model.config
                |> Maybe.withDefault []
    in
    Ui.layout modalAddition <|
        Ui.column
            [ Ui.width Ui.fill
            , Ui.height Ui.fill
            , Snackbar.view (Material.snackbar Style.palette) (\x -> Snackbar.Message x Nothing) model.snackbar
                |> Maybe.withDefault Ui.none
                |> Ui.el [ Ui.alignBottom, Ui.alignRight, Ui.padding 32 ]
                |> Ui.inFront
            ]
            [ Ui.el
                (Ui.width Ui.fill
                    :: rightMenuAddition
                )
              <|
                Widget.menuBar (Material.menuBar Style.palette)
                    { title =
                        translate Intl.Lavatrice model.context
                            |> Ui.text
                            |> Ui.el []
                    , deviceClass = Ui.Desktop
                    , openLeftSheet = Just LeftDrawerToggle
                    , openRightSheet = Just RightMenuToggle
                    , openTopSheet = Nothing
                    , primaryActions =
                        []
                    , search = Nothing
                    }
            , Ui.row [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.spacing 2, Ui.clipX ]
                [ if model.leftDrawerVisible then
                    AppWidgets.leftDrawer
                        { context = model.context
                        , selected = selectedTab
                        , goToConfig = Maybe.map (\c -> ChangeTab (toMachineConfigurationTabModel c)) model.config
                        , cyclesExpanded = model.programListExpanded
                        , toggleCycles = ProgramListToggle
                        , selectedCycle = selectedCycle
                        , cycles = cycles
                        , goToRemoteControl = ChangeTab toRemoteControl
                        , goToRemoteDeviceList = ChangeTab toRemoteDevicesList
                        , goToCycle = ChangeTabCycle
                        }

                  else
                    Ui.none
                , tabRouter
                ]
            ]
