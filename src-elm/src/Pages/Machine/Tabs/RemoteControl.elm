module Pages.Machine.Tabs.RemoteControl exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.WashingMachineConfiguration as WMC
import AppData.WashingMachineState as WMS exposing (ConnectionState(..))
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Array exposing (Array)
import Bytes.Extra as Bytes
import Chart
import Chart.Attributes
import Chart.Events
import Chart.Item
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import FontAwesome.Icon as FAIcon
import FontAwesome.Solid as SolidIcons
import Html
import Html.Attributes
import Ports as Ports
import Widget as Widget
import Widget.Material as Material


type alias Datum =
    ( Float, Float )


type alias SharedModel a =
    { a
        | connectionState : ConnectionState
        , context : Context
        , config : Maybe WMC.MachineConfiguration
        , sensorsData : Array WMS.Sensors
    }


type alias Model =
    { hoveringTemperature : List (Chart.Item.One Datum Chart.Item.Dot)
    , hoveringLevel : List (Chart.Item.One Datum Chart.Item.Dot)
    , hoveringSpeed : List (Chart.Item.One Datum Chart.Item.Dot)
    , hoveringDetergents : List (Chart.Item.One Datum Chart.Item.Bar)
    , statsExpanded : Bool
    }


buildModel : Model
buildModel =
    Model [] [] [] [] False


type Msg
    = LoadConfig String
    | SelectConfig String
    | SendConfig WMC.MachineConfiguration
    | StartProgram Int
    | RestartProgram
    | StopProgram
    | PauseProgram
    | AddCredit
    | OnHoverTemperature (List (Chart.Item.One Datum Chart.Item.Dot))
    | OnHoverLevel (List (Chart.Item.One Datum Chart.Item.Dot))
    | OnHoverSpeed (List (Chart.Item.One Datum Chart.Item.Dot))
    | OnHoverDetergent (List (Chart.Item.One Datum Chart.Item.Bar))
    | ClearAlarms
    | ToggleStats Bool


update : Msg -> SharedModel a -> Model -> ( SharedModel a, Model, Cmd msg )
update msg sharedModel model =
    case msg of
        LoadConfig archive ->
            ( sharedModel, model, Ports.getRemoteMachineConfiguration archive )

        SelectConfig archive ->
            ( sharedModel, model, Ports.selectRemoteMachineConfiguration archive )

        SendConfig archive ->
            ( sharedModel
            , model
            , Ports.sendRemoteMachineConfiguration
                archive.parmac.name
                (Array.fromList <| Bytes.toByteValues <| WMC.createArchive archive)
            )

        StartProgram index ->
            ( sharedModel, model, Ports.startProgram index )

        StopProgram ->
            ( sharedModel, model, Ports.stopProgram )

        PauseProgram ->
            ( sharedModel, model, Ports.pauseProgram )

        RestartProgram ->
            ( sharedModel, model, Ports.restartProgram )

        AddCredit ->
            ( sharedModel, model, Cmd.none )

        OnHoverTemperature hovering ->
            ( sharedModel, { model | hoveringTemperature = hovering, hoveringLevel = [], hoveringSpeed = [], hoveringDetergents = [] }, Cmd.none )

        OnHoverLevel hovering ->
            ( sharedModel, { model | hoveringLevel = hovering, hoveringTemperature = [], hoveringSpeed = [], hoveringDetergents = [] }, Cmd.none )

        OnHoverSpeed hovering ->
            ( sharedModel, { model | hoveringSpeed = hovering, hoveringLevel = [], hoveringTemperature = [], hoveringDetergents = [] }, Cmd.none )

        OnHoverDetergent hovering ->
            ( sharedModel, { model | hoveringDetergents = hovering, hoveringSpeed = [], hoveringLevel = [], hoveringTemperature = [] }, Cmd.none )

        ClearAlarms ->
            ( sharedModel, model, Ports.clearAlarms )

        ToggleStats toggled ->
            ( sharedModel, { model | statsExpanded = toggled }, Cmd.none )



-- VIEW


topPanelHeightNumber : Int
topPanelHeightNumber =
    520


chartHeightNumber : Float
chartHeightNumber =
    64.0


topPanelHeight : Ui.Length
topPanelHeight =
    Ui.px topPanelHeightNumber


view : SharedModel a -> Model -> Ui.Element Msg
view ({ connectionState, context } as sharedModel) model =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacing 16 ]
        [ Ui.paragraph [ Font.size 32, Ui.centerX ] [ Ui.text (translate Intl.Macchina context) ]
        , case connectionState of
            Connected state configuration stats ->
                machineView sharedModel model state configuration stats

            Error ->
                Ui.paragraph [ Ui.width Ui.fill ] <| [ Ui.text <| translate Intl.ErroreDiRete context ]

            Disconnected ->
                Ui.paragraph [ Ui.width Ui.fill ] <| [ Ui.text <| translate Intl.Disconnesso context ]
        ]
        |> AppWidgets.scrollbarYEl [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]


machineView : SharedModel a -> Model -> WMS.State -> WMS.Configuration -> WMS.Statistics -> Ui.Element Msg
machineView { context, config, sensorsData } { hoveringTemperature, hoveringLevel, hoveringSpeed, hoveringDetergents, statsExpanded } { state, credit, cycleNumber, stepType, portholeOpen, alarmCode, cycleRemaining, stepRemaining, stepNumber, stepCount } { machines, name, programs } stats =
    let
        washType =
            Array.get cycleNumber programs |> Maybe.map .washType |> Maybe.withDefault 0

        formatTime time =
            let
                whyIsntThisCore =
                    String.padLeft 2 '0'
            in
            (whyIsntThisCore <| String.fromInt (time // 3600)) ++ ":" ++ (whyIsntThisCore <| String.fromInt (modBy 60 (time // 60))) ++ ":" ++ (whyIsntThisCore <| String.fromInt (modBy 60 time))

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
                runningInfo button =
                    Ui.row [ Ui.centerY, Ui.width Ui.fill, Ui.spacing 16 ]
                        [ Ui.column [ Ui.alignLeft, Ui.spacing 16 ] [ Ui.text <| formatTime cycleRemaining, Ui.text <| formatTime stepRemaining ]
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
                    [ Ui.column [ Ui.centerY, Ui.spacing 32, Ui.width Ui.fill ]
                        [ Ui.paragraph [ Ui.alignLeft ]
                            [ Ui.text <| translate Intl.Allarme context ++ ": " ++ String.fromInt alarmCode ]
                        , AppWidgets.textButton
                            (translate Intl.Azzera context)
                            (Just ClearAlarms)
                        ]
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
                        chart lens color msg hovering um =
                            Chart.chart
                                [ Chart.Attributes.height chartHeightNumber

                                --,Chart.Attributes.width 480
                                --Chart.Attributes.htmlAttrs [Html.Attributes.style "height" "30%" ]
                                , Chart.Events.onMouseMove msg (Chart.Events.getNearest Chart.Item.dots)
                                , Chart.Events.onMouseLeave (msg [])
                                ]
                                [ Chart.yLabels [ Chart.Attributes.withGrid, Chart.Attributes.fontSize 8 ]
                                , Chart.series Tuple.first [ Chart.interpolated Tuple.second [ Chart.Attributes.color color ] [] ] <|
                                    Array.toList <|
                                        Array.indexedMap (\i sensors -> ( toFloat i, toFloat <| lens sensors )) sensorsData
                                , Chart.each hovering <|
                                    \_ item ->
                                        let
                                            y =
                                                Chart.Item.getY item
                                        in
                                        [ Chart.tooltip item [] [] [ Html.text (String.fromFloat y ++ " " ++ um) ] ]
                                ]
                                |> Ui.html
                                |> Ui.el [ Ui.width Ui.fill, Ui.paddingXY 48 16 ]
                    in
                    Ui.column [ Ui.width Ui.fill, Ui.spacing 32 ]
                        [ chart .temperature "red" OnHoverTemperature hoveringTemperature "°C" -- "\u{00B0}C"
                        , chart .level "blue" OnHoverLevel hoveringLevel "cm"
                        , chart .speed "green" OnHoverSpeed hoveringSpeed "rpm"
                        ]
                        |> Ui.el ([ Ui.width Ui.fill, Ui.height topPanelHeight, Ui.scrollbarY, Ui.spacing 16 ] ++ Style.focusedBorder True)

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

        statsPanel : WMS.Statistics -> Ui.Element Msg
        statsPanel s =
            Widget.expansionItem (Material.expansionItem Style.palette)
                { onToggle = ToggleStats
                , isExpanded = statsExpanded
                , icon = always Ui.none
                , text = translate Intl.Statistiche context
                , content =
                    [ ( Intl.CicliTotali, String.fromInt s.cycles )
                    , ( Intl.CicliInterrotti, String.fromInt s.interruptedCycles )
                    , ( Intl.CicliContinui, String.fromInt s.loopCycles )
                    , ( Intl.TempoAcceso, formatTime s.onTime )
                    , ( Intl.TempoDiLavoro, formatTime s.workTime )
                    , ( Intl.TempoInMoto, formatTime s.rotationTime )
                    , ( Intl.TempoInRiscaldamento, formatTime s.heatingTime )
                    , ( Intl.TempoDiErogazioneDiAcquaFredda, formatTime s.coldWaterTime )
                    , ( Intl.TempoDiErogazioneDiAcquaCalda, formatTime s.warmWaterTime )
                    , ( Intl.TempoDellAcquaDiRecupero, formatTime s.recoveryWaterTime )
                    , ( Intl.TempoDellAcquaDiFlussaggio, formatTime s.fluxWaterTime )
                    , ( Intl.ChiusureOblo, String.fromInt s.portholeClosings )
                    , ( Intl.ApertureOblo, String.fromInt s.portholeOpenings )
                    ]
                        |> List.map
                            (\( intl, datum ) ->
                                Ui.row [ Ui.width Ui.fill, Ui.padding 16 ]
                                    [ Ui.paragraph [ Ui.alignLeft ] [ Ui.text <| translate intl context ]
                                    , Ui.el [ Ui.alignRight ] <| Ui.text datum
                                    ]
                            )
                        |> (\l ->
                                l
                                    ++ [ Chart.chart
                                            [ Chart.Attributes.height 100
                                            , Chart.Attributes.padding { top = 0, bottom = 8, left = 16, right = 0 }
                                            , Chart.Events.onMouseMove OnHoverDetergent (Chart.Events.getNearest Chart.Item.bars)
                                            , Chart.Events.onMouseLeave (OnHoverDetergent [])
                                            ]
                                            [ Chart.xLabels [ Chart.Attributes.withGrid, Chart.Attributes.fontSize 6, Chart.Attributes.ints ]
                                            , Chart.yLabels [ Chart.Attributes.withGrid, Chart.Attributes.fontSize 6 ]
                                            , Chart.bars
                                                [ Chart.Attributes.x1 Tuple.first, Chart.Attributes.margin 0.02 ]
                                                [ Chart.bar Tuple.second [] ]
                                                (List.indexedMap (\i y -> ( toFloat i, toFloat y )) (List.take 10 s.soapTimes))
                                            , Chart.each hoveringDetergents <|
                                                \_ item ->
                                                    let
                                                        x =
                                                            Chart.Item.getX item

                                                        y =
                                                            Chart.Item.getY item
                                                    in
                                                    [ Chart.tooltip item [] [] [ Html.text (translate Intl.TempoDiErogazioneSapone context ++ " " ++ String.fromFloat (x + 1) ++ ": " ++ formatTime (floor y)) ] ]
                                            ]
                                            |> Ui.html
                                            |> Ui.el [ Ui.width Ui.fill, Ui.padding 32 ]
                                       ]
                           )
                        |> List.map Widget.asItem
                }
                |> Widget.itemList (Material.cardColumn Style.palette)
    in
    Ui.column [ Ui.width Ui.fill, Ui.spacing 32 ]
        [ controlPanel
        , Style.br
        , statsPanel stats
        , Style.br
        , Ui.column [ Ui.centerX, Ui.spacing 32 ]
            [ Ui.paragraph [] [ Ui.text <| translate Intl.ConfigurazioneCorrente context ++ " " ++ name ]
            , sendConfigBtn
            , archiveList machines LoadConfig SelectConfig
            ]
        , Style.br
        ]
