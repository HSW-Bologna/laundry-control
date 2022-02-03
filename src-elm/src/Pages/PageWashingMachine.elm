module Pages.PageWashingMachine exposing (..)

import AppData.WashingMachineConfiguration as WS exposing (MachineConfiguration, MachineParameters, extractArchive)
import AppWidgets.AppWidgets as AppWidgets
import Browser
import Bytes exposing (Bytes)
import Context exposing (Context, getTransl, translate)
import Element as Ui
import File exposing (File)
import File.Download as Download
import File.Select as Select
import FontAwesome.Transforms exposing (RepositionTransform(..))
import Html exposing (Html)
import Json.Decode as Decode
import Pages.WashingMachineTabs.MachineConfiguration as ParMacTab
import Pages.WashingMachineTabs.RemoteControl as RemControlTab
import Pages.WashingMachineTabs.WashCycles as WashCyclesTab
import Ports exposing (navigateHome)
import Task
import Time
import Widget as Widget
import Widget.Icon as Icon
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
    , tabModel : Maybe TabModel
    , config : Maybe MachineConfiguration
    , snackbar : Snackbar.Snackbar String
    }


init : ( Decode.Value, String ) -> ( Model, Cmd Msg )
init ( value, language ) =
    let
        initialContext =
            Context.fromJsonValue language value
    in
    ( { context = initialContext
      , leftDrawerVisible = False
      , rightMenuVisible = False
      , tabModel = Nothing
      , config = Nothing
      , snackbar = Snackbar.init
      }
    , Cmd.none
    )


type TabModel
    = MachineConfigurationModel ParMacTab.Model
    | RemoteControlModel RemControlTab.Model
    | WashCyclesModel WashCyclesTab.Model



-- UPDATE


toMachineConfiguration : Context -> MachineConfiguration -> TabModel
toMachineConfiguration context config =
    ParMacTab.buildModel context config
        |> MachineConfigurationModel


fromMachineConfiguration : ParMacTab.Model -> Model -> Model
fromMachineConfiguration tabModel model =
    { model | config = Just tabModel.config }


toWashCycles : Model -> TabModel
toWashCycles model =
    WashCyclesTab.Model model.context
        |> WashCyclesModel


toRemoteControl : Model -> TabModel
toRemoteControl model =
    RemControlTab.Model model.context
        |> RemoteControlModel


type Msg
    = Back
    | SaveMachineConfig
    | LoadMachineFromLocal
    | CreateNewMachineConfig
    | MachineSelected File
    | MachineLoaded Bytes
    | LeftDrawerToggle
    | RightMenuToggle
    | MachineConfigurationMsg ParMacTab.Msg
    | RemoteControlMsg RemControlTab.Msg
    | WashCyclesMsg WashCyclesTab.Msg
    | ChangeTab TabModel
    | TimePassed Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newMessage s m =
            { m | snackbar = Snackbar.dismiss model.snackbar |> Snackbar.insert (translate s m.context) }

        hideMenu m =
            { m | rightMenuVisible = False }

        fillTab config m =
            case m.tabModel of
                Just _ ->
                    { m | config = Just config }

                Nothing ->
                    { m | config = Just config, tabModel = Just <| toMachineConfiguration m.context config }
    in
    case ( msg, model.tabModel ) of
        ( Back, _ ) ->
            ( model, navigateHome model.context.language )

        ( TimePassed int, _ ) ->
            ( { model | snackbar = model.snackbar |> Snackbar.timePassed int }, Cmd.none )

        ( CreateNewMachineConfig, _ ) ->
            ( model
                |> newMessage "nuova_configurazione_creata"
                |> hideMenu
                |> fillTab (WS.default model.context)
            , Cmd.none
            )

        ( SaveMachineConfig, _ ) ->
            ( model
                |> newMessage "configurazione_salvata"
                |> hideMenu
            , Maybe.map (\c -> Download.bytes (c.parmac.name ++ ".WS2020.tar.gz") "application/gzip" <| WS.createArchive c) model.config
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
                        |> newMessage "configurazione_caricata"
                        |> fillTab config

                Nothing ->
                    newMessage "configurazione_non_valida" model
              )
                |> hideMenu
            , Cmd.none
            )

        ( LeftDrawerToggle, _ ) ->
            ( { model | leftDrawerVisible = not model.leftDrawerVisible } |> hideMenu, Cmd.none )

        ( RightMenuToggle, _ ) ->
            ( { model | rightMenuVisible = not model.rightMenuVisible }, Cmd.none )

        ( ChangeTab change, _ ) ->
            ( { model | tabModel = Just <| change } |> hideMenu, Cmd.none )

        ( MachineConfigurationMsg tabMsg, Just (MachineConfigurationModel tabModel) ) ->
            let
                newTabModel =
                    ParMacTab.update tabMsg tabModel
            in
            ( { model | tabModel = Just <| MachineConfigurationModel <| newTabModel }
                |> fromMachineConfiguration newTabModel
            , Cmd.none
            )

        ( RemoteControlMsg tabMsg, Just (RemoteControlModel tabModel) ) ->
            ( model, Cmd.none )

        ( WashCyclesMsg tabMsg, Just (WashCyclesModel tabModel) ) ->
            ( model, Cmd.none )

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
    Time.every 100 (always (TimePassed 100))



-- VIEW


view : Model -> Html Msg
view model =
    let
        selectedTab =
            Maybe.map
                (\x ->
                    case x of
                        MachineConfigurationModel _ ->
                            0

                        RemoteControlModel _ ->
                            1

                        WashCyclesModel _ ->
                            2
                )
                model.tabModel

        tabRouter =
            case model.tabModel of
                Just (MachineConfigurationModel tabModel) ->
                    ParMacTab.view tabModel |> Ui.map MachineConfigurationMsg

                Just (RemoteControlModel tabModel) ->
                    RemControlTab.view tabModel |> Ui.map RemoteControlMsg

                Just (WashCyclesModel tabModel) ->
                    WashCyclesTab.view tabModel |> Ui.map WashCyclesMsg

                Nothing ->
                    Ui.none

        rightMenuAddition =
            if model.rightMenuVisible then
                [ Ui.below
                    (AppWidgets.rightMenu model.context
                        [ ( "carica_configurazione", Just LoadMachineFromLocal )
                        , ( "salva_configurazione", Maybe.map (always SaveMachineConfig) model.config )
                        , ( "nuova_configurazione", Just CreateNewMachineConfig )
                        ]
                    )
                ]

            else
                []
    in
    Ui.layout [] <|
        Ui.column
            [ Ui.width Ui.fill
            , Ui.height Ui.fill
            , Snackbar.view (Material.snackbar Material.defaultPalette) (\x -> Snackbar.Message x Nothing) model.snackbar
                |> Maybe.withDefault Ui.none
                |> Ui.el [ Ui.alignBottom, Ui.alignRight, Ui.padding 32 ]
                |> Ui.inFront
            ]
            [ Ui.el
                (Ui.width Ui.fill
                    :: rightMenuAddition
                )
              <|
                Widget.menuBar (Material.menuBar Material.defaultPalette)
                    { title =
                        getTransl "lavatrice" model
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
                    AppWidgets.leftDrawer model.context
                        Back
                        selectedTab
                        [ ( "parametri_macchina", Maybe.map (\c -> ChangeTab (toMachineConfiguration model.context c)) model.config )
                        , ( "controllo_remoto", Just <| ChangeTab (toRemoteControl model) )
                        , ( "lavaggi", Just <| ChangeTab (toWashCycles model) )
                        ]

                  else
                    Ui.none
                , tabRouter
                ]
            ]
