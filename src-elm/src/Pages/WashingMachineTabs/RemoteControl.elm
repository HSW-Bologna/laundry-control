module Pages.WashingMachineTabs.RemoteControl exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.WashingMachineConfiguration as WMC
import AppData.WashingMachineState as WMS exposing (ConnectionState(..))
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Array exposing (Array)
import Bytes.Extra as Bytes
import Chart
import Chart.Attributes
import Chart.Item
import Chart.Events
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import FontAwesome.Icon as FAIcon
import FontAwesome.Solid as SolidIcons
import Ports as Ports
import Widget as Widget
import Widget.Material as Material


type alias SharedModel a =
    { a
        | connectionState : ConnectionState
        , context : Context
        , config : Maybe WMC.MachineConfiguration
        , sensorsData : Array WMS.Sensors
    }


type Msg
    = LoadConfig String
    | SelectConfig String
    | SendConfig WMC.MachineConfiguration
    | StartProgram Int
    | RestartProgram
    | StopProgram
    | PauseProgram
    | AddCredit


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

        PauseProgram ->
            ( model, Ports.pauseProgram )

        RestartProgram ->
            ( model, Ports.restartProgram )

        AddCredit ->
            ( model, Cmd.none )



-- VIEW


topPanelHeightNumber : Int
topPanelHeightNumber =
    520


chartHeightNumber : Float
chartHeightNumber =
    64.0


topPanelHeight : Ui.Length
topPanelHeight =
    Ui.px 520


view : SharedModel a -> Ui.Element Msg
view ({ connectionState, context } as model) =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacing 16 ]
        [ Ui.paragraph [ Font.size 32, Ui.centerX ] [ Ui.text (translate Intl.ControlloRemoto context) ]
        , case connectionState of
            Connected state configuration ->
                machineView model state configuration

            Error ->
                Ui.paragraph [ Ui.width Ui.fill ] <| [ Ui.text <| translate Intl.ErroreDiRete context ]

            Disconnected ->
                Ui.paragraph [ Ui.width Ui.fill ] <| [ Ui.text <| translate Intl.Disconnesso context ]
        ]
        |> AppWidgets.scrollbarYEl [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]


machineView : SharedModel a -> WMS.State -> WMS.Configuration -> Ui.Element Msg
machineView { context, config, sensorsData } { state, credit, cycleNumber, stepType, portholeOpen, alarmCode, cycleRemaining, stepRemaining, stepNumber, stepCount } { machines, name, programs } =
    let
        washType =
            Array.get cycleNumber programs |> Maybe.map .washType |> Maybe.withDefault 0

        cardStyle =
            [ Ui.height Ui.fill, Ui.width Ui.fill, Ui.padding 8, Ui.spacing 8 ]

        programList : Array WMS.ProgramPreview -> (Int -> msg) -> Ui.Element msg
        programList ps start =
            let
                programOption i program =
                    Ui.row [ Ui.width Ui.fill, Ui.spacing 8 ]
                        [ Ui.paragraph [ Ui.width Ui.fill, Ui.alignLeft ] [ Ui.text <| String.fromInt (i + 1) ++ " " ++ program.name ]
                        , AppWidgets.washTypeImage program.washType 32
                        , AppWidgets.iconButton SolidIcons.play (start i) "start"
                        ]
                        |> Widget.asItem
            in
            Array.indexedMap programOption ps
                |> Array.toList
                |> Widget.itemList (Material.cardColumn Style.palette)

        --|> Ui.column [ Ui.width Ui.fill ]
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

        statusCard : Ui.Element Msg
        statusCard =
            let
                portholeState =
                    Ui.text <|
                        translate
                            (if portholeOpen then
                                Intl.ObloAperto

                             else
                                Intl.ObloChiuso
                            )
                            context

                runningStatus t =
                    Ui.row [ Ui.width Ui.fill, Ui.centerY ]
                        [ Ui.column [ Ui.spaceEvenly, Ui.height Ui.fill, Ui.centerY ]
                            [ Ui.text <| translate t context
                            , portholeState
                            , Ui.text <| WMC.washStepTypeToString context stepType ++ " " ++ String.fromInt (stepNumber + 1) ++ "/" ++ String.fromInt stepCount
                            ]
                        , Ui.el [ Ui.alignRight ] <| AppWidgets.washTypeImage washType 120
                        ]
            in
            (case state of
                WMS.Stopped ->
                    Ui.row [ Ui.width Ui.fill ]
                        [ Ui.column [ Ui.spaceEvenly, Ui.height Ui.fill, Ui.centerY ]
                            [ Ui.text <|
                                translate Intl.MacchinaFerma
                                    context
                            , portholeState
                            ]
                        , Ui.row [ Ui.alignRight, Ui.spacing 12 ] [ AppWidgets.iconButton SolidIcons.coins AddCredit "credit", Ui.text <| String.fromInt credit ]
                        ]

                WMS.Running ->
                    runningStatus Intl.LavaggioInCorso

                WMS.Paused ->
                    runningStatus Intl.LavaggioInPausa

                WMS.Braking ->
                    runningStatus Intl.FrenataInCorso

                WMS.ForcedDrain ->
                    runningStatus Intl.ScaricoInCorso
            )
                |> Ui.el (Style.focusedBorder True ++ cardStyle)

        infoCard : Ui.Element Msg
        infoCard =
            let
                whyIsntThisCore =
                    String.padLeft 2 '0'

                formatTime time =
                    (whyIsntThisCore <| String.fromInt (time // 3600)) ++ ":" ++ (whyIsntThisCore <| String.fromInt (modBy 60 (time // 60))) ++ ":" ++ (whyIsntThisCore <| String.fromInt (modBy 60 time))

                runningInfo button =
                    Ui.row [ Ui.centerY, Ui.width Ui.fill, Ui.spacing 16 ]
                        [ FAIcon.viewIcon SolidIcons.clock |> Ui.html
                        , Ui.column [ Ui.alignLeft, Ui.spacing 16 ] [ Ui.text <| formatTime cycleRemaining, Ui.text <| formatTime stepRemaining ]
                        , Ui.column [ Ui.alignRight, Ui.spacing 16 ]
                            [ button
                            , AppWidgets.iconButton SolidIcons.stop StopProgram "stop"
                            ]
                        ]
                        |> Ui.el (Style.focusedBorder True ++ cardStyle)
            in
            case state of
                WMS.Stopped ->
                    Ui.el cardStyle Ui.none

                WMS.Running ->
                    runningInfo <| AppWidgets.iconButton SolidIcons.pause PauseProgram "pause"

                _ ->
                    runningInfo <| AppWidgets.iconButton SolidIcons.play RestartProgram "start"

        warningCard : Ui.Element Msg
        warningCard =
            (if alarmCode > 0 then
                Ui.row [ Ui.width Ui.fill, Ui.centerY ]
                    [ Ui.paragraph [ Ui.alignLeft ] [ Ui.text <| translate Intl.Allarme context ++ ": " ++ String.fromInt alarmCode ]
                    , Ui.image [ Ui.alignRight ] { src = "images/warning.png", description = "warning" }
                    ]

             else
                Ui.none
            )
                |> Ui.el (Style.focusedBorder (alarmCode > 0) ++ cardStyle)

        leftControlColumn : Ui.Element Msg
        leftControlColumn =
            case state of
                WMS.Stopped ->
                    programList programs StartProgram
                        |> Ui.el [ Ui.width Ui.fill, Ui.height topPanelHeight, Ui.scrollbarY, Ui.spacing 16 ]

                _ ->
                    let
                        chart lens color =
                            Chart.chart
                                [ Chart.Attributes.height chartHeightNumber ]
                                [ Chart.yLabels [ Chart.Attributes.withGrid, Chart.Attributes.fontSize 8 ]
                                , Chart.series Tuple.first [ Chart.interpolated Tuple.second [ Chart.Attributes.color color ] [ Chart.Attributes.circle ] ] <|
                                    Array.toList <|
                                        Array.indexedMap (\i sensors -> ( toFloat i, toFloat <| lens sensors )) sensorsData
                                ]
                                |> Ui.html
                                |> Ui.el [ Ui.width Ui.fill, Ui.padding 12 ]
                    in
                    Ui.column [ Ui.width Ui.fill, Ui.height topPanelHeight, Ui.spacing 32 ]
                        [ chart .temperature "red"
                        , chart .level "blue"
                        , chart .speed "green"
                        ]

        --|> Ui.el (Style.focusedBorder (alarmCode > 0) ++ cardStyle)
        rightControlColumn : Ui.Element Msg
        rightControlColumn =
            Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.spacing 16 ]
                [ statusCard
                , infoCard
                , warningCard
                ]

        controlPanel : Ui.Element Msg
        controlPanel =
            Ui.row
                [ Ui.width Ui.fill, Ui.height topPanelHeight, Ui.spacing 32 ]
                [ leftControlColumn
                , rightControlColumn
                ]
    in
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.spacing 32 ]
        [ controlPanel
        , Style.br
        , Ui.column [ Ui.centerX, Ui.height <| Ui.maximum topPanelHeightNumber Ui.fill, Ui.scrollbarY, Ui.spacing 32 ]
            [ Ui.paragraph [] [ Ui.text <| translate Intl.ConfigurazioneCorrente context ++ " " ++ name ]
            , sendConfigBtn
            , archiveList machines LoadConfig SelectConfig
            ]
        ]
