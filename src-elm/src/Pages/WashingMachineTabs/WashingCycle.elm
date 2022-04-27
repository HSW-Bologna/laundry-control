module Pages.WashingMachineTabs.WashingCycle exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (getTranslation)
import AppData.Parameter as Parameter exposing (formatPrice)
import AppData.WashingMachineConfiguration as WMC exposing (MachineParameters, WashingCycle, changeWashCycleName)
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Array exposing (Array)
import Array.Extra as Array
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Solid as SolidIcons
import FontAwesome.Svg as FontAwesomeSvg
import Round
import Widget as Widget
import Widget.Icon as Icon
import Widget.Material as Material


type TabCmd
    = None
    | MoveUp Int
    | MoveDown Int
    | Copy WashingCycle Int
    | Remove Int


type alias SharedModel a =
    { a
        | context : Context
    }


type alias Model =
    { index : Int
    , cycle : WashingCycle
    , parmac : MachineParameters
    , priceString : String
    , removeDialog : Bool
    , newStepDialog : Maybe StepMetadata
    , removeStepDialog : Maybe Int
    , expandedSteps : Array Bool
    , selected : Maybe ( Int, WMC.WashParameter, AppWidgets.ParameterModificationData )
    , focused : Maybe Int
    }


type alias StepMetadata =
    { index : Int
    , stepType : Int
    , energetic : Bool
    }


buildModel : Int -> WashingCycle -> MachineParameters -> Model
buildModel index cycle parmac =
    let
        price =
            formatPrice parmac.priceDecimalDigits cycle.price
    in
    Model index cycle parmac price False Nothing Nothing Array.empty Nothing Nothing


validatePriceString : MachineParameters -> String -> Maybe String
validatePriceString { priceDecimalDigits } string =
    case string of
        "" ->
            Just ""

        value ->
            String.toFloat value
                |> Maybe.map (Round.round priceDecimalDigits)


validatePrice : MachineParameters -> String -> Maybe Int
validatePrice { priceDecimalDigits } string =
    case string of
        "" ->
            Nothing

        value ->
            String.toFloat value
                |> Maybe.map ((*) (toFloat priceDecimalDigits))
                |> Maybe.map round


type Msg
    = ConfigNameChange String
    | ChangeType Int
    | PriceChange String
    | TabMoveUp
    | TabMoveDown
    | TabRemove
    | AskToRemove
    | TabCopy
    | Dismiss
    | MoveStepUp Int
    | MoveStepDown Int
    | AskToRemoveStep Int
    | RemoveStep Int
    | CopyStep Int
    | AskNewStep
    | ChangeNewStepMetadata StepMetadata
    | ExpandStep Int Bool
    | SelectParameter Int WMC.WashParameter
    | UnselectParameter
    | ParameterChange String
    | ParameterConfirm Int WMC.WashParameter Int
    | NewStep Int WMC.WashingStep


update : Msg -> SharedModel a -> Model -> ( Model, TabCmd )
update msg { context } ({ cycle, index, parmac } as model) =
    let
        resize a i =
            if Array.length a < i + 1 then
                Array.resizelRepeat (i + 1) False a

            else
                a

        toggleExpanded m i b =
            { m | expandedSteps = Array.set i b <| resize m.expandedSteps i, focused = Just i }

        focusPrev i m =
            if i > 0 then
                { m | focused = Just (i - 1) }

            else
                m

        focusNext i m =
            if i + 1 < Array.length m.cycle.steps then
                { m | focused = Just (i + 1) }

            else
                m
    in
    case msg of
        SelectParameter stepIndex par ->
            let
                washingStep =
                    Array.get stepIndex model.cycle.steps
            in
            ( Maybe.map (\step -> { model | selected = Just ( stepIndex, par, AppWidgets.parameterModificationData step parmac context par ) }) washingStep
                |> Maybe.withDefault model
            , None
            )

        UnselectParameter ->
            ( { model | selected = Nothing }, None )

        ParameterChange text ->
            ( { model | selected = Maybe.map (\( si, par, data ) -> ( si, par, AppWidgets.validateParameterInput text data )) model.selected }, None )

        ParameterConfirm stepIndex par value ->
            ( { model | cycle = { cycle | steps = Array.update stepIndex (\s -> par.set value s) cycle.steps }, selected = Nothing }
            , None
            )

        ConfigNameChange name ->
            ( { model | cycle = changeWashCycleName name context.language cycle }, None )

        ChangeType selected ->
            ( { model | cycle = { cycle | washType = selected } }, None )

        PriceChange string ->
            let
                newPriceString =
                    validatePriceString model.parmac string |> Maybe.withDefault model.priceString

                priceInt =
                    validatePrice model.parmac string |> Maybe.withDefault cycle.price
            in
            ( { model | priceString = newPriceString, cycle = { cycle | price = priceInt } }, None )

        TabMoveUp ->
            ( model, MoveUp index )

        TabMoveDown ->
            ( model, MoveDown index )

        AskToRemove ->
            ( { model | removeDialog = True }, None )

        Dismiss ->
            ( { model | removeDialog = False, removeStepDialog = Nothing, newStepDialog = Nothing }, None )

        TabRemove ->
            ( model, Remove index )

        TabCopy ->
            ( model, Copy cycle index )

        MoveStepUp stepIndex ->
            ( { model
                | cycle = WMC.swapWashSteps stepIndex (stepIndex - 1) cycle
                , expandedSteps = Tuple.first <| WMC.swapElements stepIndex (stepIndex - 1) <| resize model.expandedSteps stepIndex
              }
                |> focusPrev stepIndex
            , None
            )

        MoveStepDown stepIndex ->
            ( { model
                | cycle = WMC.swapWashSteps stepIndex (stepIndex + 1) cycle
                , expandedSteps = Tuple.first <| WMC.swapElements stepIndex (stepIndex + 1) <| resize model.expandedSteps (stepIndex + 1)
              }
                |> focusNext stepIndex
            , None
            )

        AskToRemoveStep stepIndex ->
            ( { model | removeStepDialog = Just stepIndex }, None )

        RemoveStep stepIndex ->
            ( { model
                | cycle = { cycle | steps = Array.removeAt stepIndex cycle.steps }
                , expandedSteps = Array.removeAt stepIndex model.expandedSteps
                , removeStepDialog = Nothing
                , focused = Nothing
              }
            , None
            )

        CopyStep stepIndex ->
            ( Array.get stepIndex cycle.steps
                |> Maybe.map
                    (\unwrappedStep ->
                        { model
                            | cycle = { cycle | steps = Array.insertAt (stepIndex + 1) unwrappedStep cycle.steps }
                            , expandedSteps = Array.insertAt (stepIndex + 1) False model.expandedSteps
                        }
                    )
                |> Maybe.withDefault model
            , None
            )

        ExpandStep num bool ->
            ( toggleExpanded model num bool, None )

        AskNewStep ->
            ( { model | newStepDialog = Just { index = 0, stepType = 1, energetic = False } }, None )

        ChangeNewStepMetadata metadata ->
            ( { model | newStepDialog = Just metadata }, None )

        NewStep newStepIndex step ->
            ( { model
                | newStepDialog = Nothing
                , cycle =
                    { cycle | steps = Array.insertAt newStepIndex step cycle.steps }
                , expandedSteps = resize model.expandedSteps (newStepIndex + 1) |> Array.insertAt newStepIndex False
                , focused = Just newStepIndex
              }
            , None
            )



-- VIEW


controlPad : msg -> msg -> msg -> msg -> Ui.Element msg
controlPad moveUp moveDown remove copy =
    Ui.row
        [ Ui.centerY ]
        [ Ui.column [ Ui.alignRight, Ui.centerY ]
            [ AppWidgets.iconButton SolidIcons.arrowUp moveUp "up"
            , AppWidgets.iconButton SolidIcons.arrowDown moveDown "down"
            ]
        , Ui.column [ Ui.centerY, Ui.alignRight ]
            [ AppWidgets.iconButton SolidIcons.trash remove "remove"
            , AppWidgets.iconButton SolidIcons.copy copy "copy"
            ]
        ]


newStepModal : Context -> StepMetadata -> Int -> Ui.Element Msg
newStepModal context metadata numSteps =
    let
        prevIndex =
            if metadata.index > 0 then
                metadata.index - 1

            else
                metadata.index

        nextIndex =
            if metadata.index < numSteps then
                metadata.index + 1

            else
                metadata.index
    in
    Ui.column (Style.modal 480)
        [ translate Intl.NuovoPasso context
            ++ " "
            ++ String.fromInt (metadata.index + 1)
            ++ ": "
            ++ WMC.washStepTypeToString context metadata.stepType
            |> Ui.text
            |> List.singleton
            |> Ui.paragraph []
        , Ui.row [ Ui.centerX, Ui.spacing 32 ]
            [ Ui.el [ Ui.width <| Ui.px 160 ] <|
                Ui.text <|
                    translate
                        (if metadata.energetic then
                            Intl.Energetico

                         else
                            Intl.Delicato
                        )
                        context
            , Widget.switch (Material.switch Style.palette)
                { description = "tipo"
                , onPress = Just <| ChangeNewStepMetadata { metadata | energetic = not metadata.energetic }
                , active = metadata.energetic
                }
            ]
        , AppWidgets.stringChoice
            (\t -> ChangeNewStepMetadata { metadata | stepType = t + 1 })
            (WMC.washStepStrings context)
            (metadata.stepType - 1)
            |> Ui.el [ Ui.width Ui.fill ]
        , Ui.row [ Ui.width Ui.fill ]
            [ Ui.el [ Ui.width <| Ui.fillPortion 2 ] <| Ui.text (translate Intl.Posizione context)
            , Ui.row [ Ui.width <| Ui.fillPortion 1, Ui.spaceEvenly ]
                [ AppWidgets.iconButton SolidIcons.minus (ChangeNewStepMetadata { metadata | index = prevIndex }) "minus"
                , Ui.text <| String.fromInt (metadata.index + 1)
                , AppWidgets.iconButton SolidIcons.plus (ChangeNewStepMetadata { metadata | index = nextIndex }) "plus"
                ]
            ]
        , Ui.row [ Ui.width Ui.fill ]
            [ Ui.el [ Ui.alignLeft ] <|
                AppWidgets.textButton (translate Intl.Cancella context)
                    (Just Dismiss)
            , Ui.el [ Ui.alignRight ] <|
                AppWidgets.textButton (translate Intl.Conferma context) (Just (NewStep metadata.index (WMC.defaultWashStep metadata.stepType metadata.energetic)))
            ]
        ]


view : SharedModel a -> Model -> Ui.Element Msg
view { context } { focused, index, cycle, parmac, priceString, removeDialog, removeStepDialog, newStepDialog, expandedSteps, selected } =
    let
        modals =
            (if removeDialog then
                [ Widget.dialog (Material.alertDialog Style.palette)
                    { title = Just <| translate Intl.Rimuovi context
                    , text = translate Intl.RimuovereIlLavaggio context
                    , accept = Just <| { text = translate Intl.Conferma context, onPress = Just TabRemove }
                    , dismiss = Just <| { text = translate Intl.Cancella context, onPress = Just Dismiss }
                    }
                ]

             else
                []
            )
                ++ (Maybe.map
                        (\md ->
                            [ { onDismiss = Just Dismiss
                              , content = newStepModal context md (Array.length cycle.steps)
                              }
                            ]
                        )
                        newStepDialog
                        |> Maybe.withDefault []
                   )
                ++ (Maybe.map
                        (\i ->
                            [ Widget.dialog (Material.alertDialog Style.palette)
                                { title = Just <| translate Intl.Rimuovi context
                                , text = translate Intl.RimuovereIlPasso context
                                , accept = Just <| { text = translate Intl.Conferma context, onPress = Just <| RemoveStep i }
                                , dismiss = Just <| { text = translate Intl.Cancella context, onPress = Just Dismiss }
                                }
                            ]
                        )
                        removeStepDialog
                        |> Maybe.withDefault []
                   )
                ++ (Maybe.map
                        (\( i, p, t ) ->
                            [ { onDismiss = Just UnselectParameter
                              , content =
                                    AppWidgets.parameterModificationDialog
                                        { info = parmac
                                        , context = context
                                        , textChange = ParameterChange
                                        , dismiss = UnselectParameter
                                        , confirm = ParameterConfirm i
                                        , modData = t
                                        , par = p
                                        , priceMultiplier = parmac.priceDecimalDigits
                                        }
                              }
                            ]
                        )
                        selected
                        |> Maybe.withDefault []
                   )
                |> Widget.singleModal

        priceLabel =
            Ui.text <| translate Intl.Prezzo context

        name =
            getTranslation context.language cycle.name

        stepWidget i s =
            Ui.row (Ui.width Ui.fill :: Style.focusedBorder (Maybe.map (\f -> i == f) focused |> Maybe.withDefault False))
                [ Ui.el [ Ui.width Ui.fill, Ui.alignTop ] <|
                    AppWidgets.step context parmac ExpandStep SelectParameter (Array.get i expandedSteps |> Maybe.withDefault False) i s
                , Ui.el [ Ui.alignTop ] <| controlPad (MoveStepUp i) (MoveStepDown i) (AskToRemoveStep i) (CopyStep i)
                ]
    in
    AppWidgets.scrollbarYEl (modals ++ [ Ui.width Ui.fill, Ui.height Ui.fill ]) <|
        Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacingXY 0 16, Ui.scrollbarY ]
            ([ Ui.row [ Ui.width Ui.fill, Ui.spacing 32 ]
                [ Ui.column [ Ui.width Ui.fill, Ui.spacing 8 ]
                    [ Input.text [ Ui.alignLeft, Ui.width Ui.fill ]
                        { onChange = ConfigNameChange
                        , text = name
                        , placeholder = Nothing
                        , label = Input.labelHidden "name"
                        }
                    , Input.text [ Ui.width Ui.fill ]
                        { onChange = PriceChange
                        , text = priceString
                        , placeholder = Just <| Input.placeholder [] priceLabel
                        , label = Input.labelLeft [] priceLabel
                        }
                    ]
                , Ui.row []
                    [ Ui.el [ Font.size 64, Ui.centerY, Ui.alignLeft ] <|
                        Ui.text <|
                            String.fromInt (index + 1)
                    , controlPad TabMoveUp TabMoveDown AskToRemove TabCopy
                    ]
                ]
             , Ui.row [ Ui.width Ui.fill, Ui.spacing 32, Ui.paddingEach { top = 0, bottom = 0, left = 0, right = 32 } ]
                [ AppWidgets.stringChoice ChangeType (WMC.washTypesStrings context) cycle.washType |> Ui.el [ Ui.width Ui.fill, Ui.alignLeft ]
                , AppWidgets.washTypeImage cycle.washType 160
                ]
             , Style.br
             , Ui.el [ Ui.centerX, Ui.paddingEach { top = 0, left = 16, right = 16, bottom = 32 } ] <|
                Widget.button (Material.containedButton Style.palette)
                    { text = translate Intl.NuovoPasso context
                    , icon = SolidIcons.plus |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
                    , onPress =
                        if Array.length cycle.steps < 36 then
                            Just AskNewStep

                        else
                            Nothing
                    }
             ]
                ++ (cycle.steps
                        |> Array.indexedMap stepWidget
                        |> Array.toList
                   )
            )
