module Pages.WashingMachineTabs.RemoteControl exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.WashingMachineConfiguration as WMC
import AppData.WashingMachineState as WMS exposing (ConnectionState(..))
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Array exposing (Array)
import Bytes.Extra as Bytes
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import FontAwesome.Solid as SolidIcons
import Ports as Ports
import Widget as Widget
import Widget.Material as Material


type alias SharedModel a =
    { a
        | connectionState : ConnectionState
        , context : Context
        , config : Maybe WMC.MachineConfiguration
    }


type Msg
    = LoadConfig String
    | SelectConfig String
    | SendConfig WMC.MachineConfiguration
    | StartProgram Int
    | StopProgram


update : Msg -> SharedModel a -> ( SharedModel a, Cmd msg )
update msg model =
    case msg of
        LoadConfig archive ->
            ( model, Ports.getRemoteMachineConfiguration archive )

        SelectConfig archive ->
            ( model, Ports.selectRemoteMachineConfiguration archive )

        SendConfig archive ->
            ( model
            , Ports.sendRemoteMachineConfiguration
                archive.parmac.name
                (Array.fromList <| Bytes.toByteValues <| WMC.createArchive archive)
            )

        StartProgram index ->
            ( model, Ports.startProgram index )

        StopProgram ->
            ( model, Ports.stopProgram )



-- VIEW


view : SharedModel a -> Ui.Element Msg
view ({ connectionState, context } as model) =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
        [ Ui.paragraph [ Font.size 32 ] [ Ui.text (translate Intl.ControlloRemoto context) ]
        , Ui.paragraph [ Ui.width Ui.fill ]
            [ case connectionState of
                Connected state configuration ->
                    machineView model state configuration

                Error error ->
                    Ui.text <| "Errore: " ++ error

                Disconnected ->
                    Ui.text <| "Disconnesso"
            ]
        ]
        |> AppWidgets.scrollbarYEl [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]


machineView : SharedModel a -> WMS.State -> WMS.Configuration -> Ui.Element Msg
machineView { context, config } { state } { machines, name, programs } =
    let
        programList : Array WMS.ProgramPreview -> (Int -> msg) -> Ui.Element msg
        programList ps start =
            let
                programOption i program =
                    Ui.row [ Ui.width Ui.fill, Ui.spacing 8 ]
                        [ Ui.paragraph [ Ui.width Ui.fill, Ui.alignLeft ] [ Ui.text program.name ]
                        , AppWidgets.washTypeImage program.washType 32
                        , AppWidgets.iconButton SolidIcons.play (start i) "start"
                        ]
                        |> Widget.asItem
            in
            Array.indexedMap programOption ps
                |> Array.toList
                |> Widget.itemList (Material.cardColumn Style.palette)

        archiveList : List String -> (String -> msg) -> (String -> msg) -> Ui.Element msg
        archiveList archives download select =
            let
                archiveOption archiveName =
                    Ui.row [ Ui.width Ui.fill, Ui.spacing 8 ]
                        [ Ui.paragraph [ Ui.width Ui.fill, Ui.alignLeft ] [ Ui.text archiveName ]
                        , AppWidgets.iconButton SolidIcons.download (download archiveName) "download"
                        , AppWidgets.iconButton SolidIcons.check (select archiveName) "select"
                        ]
                        |> Widget.asItem
            in
            List.map archiveOption archives
                |> Widget.itemList (Material.cardColumn Style.palette)

        sendConfigBtn =
            Maybe.map
                (\c ->
                    AppWidgets.textButton (translate Intl.InviaConfigurazione context ++ " " ++ c.parmac.name) (Just <| SendConfig c)
                )
                config
                |> Maybe.withDefault Ui.none
    in
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill ]
        [ case state of
            WMS.Running ->
                AppWidgets.textButton (translate Intl.Ferma context) (Just StopProgram)

            WMS.Stopped ->
                programList programs StartProgram
        , Ui.column []
            [ Ui.paragraph [] [ Ui.text <| translate Intl.ConfigurazioneCorrente context ++ " " ++ name ]
            , sendConfigBtn
            , archiveList machines LoadConfig SelectConfig
            ]
        ]
