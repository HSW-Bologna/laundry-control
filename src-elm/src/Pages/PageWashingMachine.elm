port module Pages.PageWashingMachine exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (getTranslation, languageFromString)
import AppData.IpAddress as IpAddress exposing (IpAddress, changePart, localhost, toString)
import AppData.WashingMachineConfiguration as WMC exposing (MachineConfiguration, MachineParameters, extractArchive)
import AppData.WashingMachineState as WSS
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Array exposing (Array)
import Array.Extra as Array
import Browser
import Bytes exposing (Bytes)
import Context exposing (Context, translate)
import Element as Ui
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FontAwesome.Transforms exposing (RepositionTransform(..))
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.WashingMachineTabs.MachineConfiguration as ParMacTab
import Pages.WashingMachineTabs.RemoteControl as RemControlTab
import Pages.WashingMachineTabs.WashingCycle as WashCycleTab
import Ports as Ports exposing (decodeEvent)
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
    , tabModel : Maybe TabModel
    , config : Maybe MachineConfiguration
    , snackbar : Snackbar.Snackbar String
    , connectionState : WSS.ConnectionState
    , localMachines : List IpAddress
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
      , tabModel = Nothing
      , config = Nothing
      , snackbar = Snackbar.init
      , connectionState = WSS.Disconnected
      , localMachines = []
      }
    , Cmd.none
    )


type TabModel
    = MachineConfigurationModel ParMacTab.Model
    | RemoteControlModel
    | WashCyclesModel WashCycleTab.Model


type VisibleModal
    = NoModal
    | IpInput IpAddress



-- UPDATE


toMachineConfigurationTabModel : Context -> MachineConfiguration -> TabModel
toMachineConfigurationTabModel context config =
    ParMacTab.buildModel context config
        |> MachineConfigurationModel


toWashingCycleTabModel : Context -> Int -> WMC.WashingCycle -> WMC.MachineParameters -> TabModel
toWashingCycleTabModel context index cycle parmac =
    WashCycleTab.buildModel context index cycle parmac
        |> WashCyclesModel


fromMachineConfigurationTabModel : ParMacTab.Model -> Model -> Model
fromMachineConfigurationTabModel tabModel model =
    { model | config = Just tabModel.config, tabModel = Just <| MachineConfigurationModel <| tabModel }


fromWashingCycleTabModel : WashCycleTab.Model -> Model -> Model
fromWashingCycleTabModel tabModel model =
    let
        newConfig =
            Maybe.map (\c -> { c | cycles = Array.update tabModel.index (always tabModel.cycle) c.cycles }) model.config
    in
    { model | tabModel = Just <| WashCyclesModel <| tabModel, config = newConfig }


toRemoteControl : TabModel
toRemoteControl =
    RemoteControlModel


type Msg
    = Back
    | SaveMachineConfig
    | LoadMachineFromLocal
    | CreateNewMachineConfig
    | MachineSelected File
    | MachineLoaded Bytes
    | LeftDrawerToggle
    | RightMenuToggle
    | ProgramListToggle Bool
    | MachineConfigurationMsg ParMacTab.Msg
    | RemoteControlMsg RemControlTab.Msg
    | WashCyclesMsg WashCycleTab.Msg
    | ChangeTab TabModel
    | ChangeTabCycle Int
    | TimePassed Int
    | StateUpdate Encode.Value
    | IpAddresses Encode.Value
    | InsertIpAddress
    | IpAddessChange IpAddress
    | LocalConnectionRequest (Maybe IpAddress)


port washingMachineHttpConnect : String -> Cmd msg


port ipAddresses : (Encode.Value -> msg) -> Sub msg


port stateUpdate : (Encode.Value -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        closeTab m =
            { m | tabModel = Nothing }

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
                    { m | config = Just config, tabModel = Just <| toMachineConfigurationTabModel m.context config }
            in
            case m.tabModel of
                Just (MachineConfigurationModel _) ->
                    updatedModel

                Nothing ->
                    updatedModel

                Just _ ->
                    { m | config = Just config }

        fillTabWithConnection connection m =
            case m.tabModel of
                Just _ ->
                    { m | connectionState = connection }

                Nothing ->
                    { m | connectionState = connection, tabModel = Just <| toRemoteControl }
    in
    case ( msg, model.tabModel ) of
        -- Global messages
        ( Back, _ ) ->
            ( model, Ports.navigateHome model.context.language )

        ( TimePassed int, _ ) ->
            ( { model | snackbar = model.snackbar |> Snackbar.timePassed int }, Cmd.none )

        ( IpAddresses value, _ ) ->
            ( decodeEvent "ipAddresses" IpAddress.listDecoder value
                |> Result.toMaybe
                |> Maybe.map (\ips -> { model | localMachines = ips })
                |> Maybe.withDefault model
            , Cmd.none
            )

        ( StateUpdate state, _ ) ->
            case decodeEvent "stateUpdate" WSS.connectionStateUpdateDecoder state of
                Ok res ->
                    ( model |> fillTabWithConnection res, Cmd.none )

                Err error ->
                    ( newRawMessage (Decode.errorToString error) model, Cmd.none )

        ( InsertIpAddress, _ ) ->
            ( { model | visibleModal = IpInput localhost }, Ports.searchMachines () )

        ( IpAddessChange ip, _ ) ->
            ( { model | visibleModal = IpInput ip }, Cmd.none )

        ( LocalConnectionRequest (Just ip), _ ) ->
            ( model |> hideModal |> hideMenu, washingMachineHttpConnect (toString ip) )

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
            ( { model | tabModel = Just change } |> hideMenu, Cmd.none )

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
                            { model | config = Just newConfig, tabModel = Just <| toWashingCycleTabModel model.context index unwrappedCycle newConfig.parmac }
                                |> hideMenu
                        )
                        cycle
                        |> Maybe.withDefault model
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        -- Machine Configuration tab messages
        ( MachineConfigurationMsg tabMsg, Just (MachineConfigurationModel tabModel) ) ->
            ( fromMachineConfigurationTabModel (ParMacTab.update tabMsg tabModel) model
            , Cmd.none
            )

        -- Remote Control tab messages
        ( RemoteControlMsg _, Just RemoteControlModel ) ->
            ( model, Cmd.none )

        -- Wash Cycle parameters tab messages
        ( WashCyclesMsg tabMsg, Just (WashCyclesModel tabModel) ) ->
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
            ( case WashCycleTab.update tabMsg tabModel of
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
        ( MachineConfigurationMsg _, Just _ ) ->
            ( model, Cmd.none )

        ( RemoteControlMsg _, Just _ ) ->
            ( model, Cmd.none )

        ( WashCyclesMsg _, Just _ ) ->
            ( model, Cmd.none )

        ( _, Nothing ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 100 (always (TimePassed 100))
        , stateUpdate StateUpdate
        , ipAddresses IpAddresses
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        selectedTab =
            Maybe.andThen
                (\x ->
                    case x of
                        MachineConfigurationModel _ ->
                            Just 1

                        RemoteControlModel ->
                            Just 0

                        WashCyclesModel _ ->
                            Nothing
                )
                model.tabModel

        selectedCycle =
            Maybe.andThen
                (\x ->
                    case x of
                        MachineConfigurationModel _ ->
                            Nothing

                        RemoteControlModel ->
                            Nothing

                        WashCyclesModel { index } ->
                            Just index
                )
                model.tabModel

        tabRouter =
            case model.tabModel of
                Just (MachineConfigurationModel tabModel) ->
                    ParMacTab.view tabModel |> Ui.map MachineConfigurationMsg

                Just RemoteControlModel ->
                    RemControlTab.view model |> Ui.map RemoteControlMsg

                Just (WashCyclesModel tabModel) ->
                    WashCycleTab.view tabModel |> Ui.map WashCyclesMsg

                Nothing ->
                    Ui.el [ Ui.centerX, Ui.centerY ] menuOptions

        menuOptions =
            AppWidgets.rightMenu model.context
                [ ( Intl.CaricaConfigurazione, Just LoadMachineFromLocal )
                , ( Intl.SalvaConfigurazione, Maybe.map (always SaveMachineConfig) model.config )
                , ( Intl.NuovaConfigurazione, Just CreateNewMachineConfig )
                , ( Intl.ConnessioneLocale, Just InsertIpAddress )
                , ( Intl.ConnessioneRemota, Nothing )
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
                    AppWidgets.ipDialog model.context model.localMachines ip IpAddessChange LocalConnectionRequest

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
            , Ui.row [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.spacing 2 ]
                [ if model.leftDrawerVisible then
                    AppWidgets.leftDrawer
                        { context = model.context
                        , selected = selectedTab
                        , back = Back
                        , goToConfig = Maybe.map (\c -> ChangeTab (toMachineConfigurationTabModel model.context c)) model.config
                        , cyclesExpanded = model.programListExpanded
                        , toggleCycles = ProgramListToggle
                        , selectedCycle = selectedCycle
                        , cycles = cycles
                        , goToRemoteControl = ChangeTab toRemoteControl
                        , goToCycle = ChangeTabCycle
                        }

                  else
                    Ui.none
                , tabRouter
                ]
            ]
