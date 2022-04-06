module Pages.WashingMachineTabs.WashCycles exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (getTranslation)
import AppData.Parameter exposing (formatPrice)
import AppData.WashingMachineConfiguration as WMC exposing (MachineParameters, WashingCycle, changeWashCycleName)
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Array exposing (Array)
import Array.Extra
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
    | Close



--| Swap Int Int
--| Copy Int
-- TODO: go back to having a fixed cycle instead of an array


type alias Model =
    { context : Context
    , index : Int
    , cycles : Array WashingCycle
    , parmac : MachineParameters
    , priceString : String
    , removeDialog : Bool
    , expandedSteps : Array Bool
    }


buildModel : Context -> Int -> Array WashingCycle -> MachineParameters -> Model
buildModel context index cycles parmac =
    let
        price =
            Array.get index cycles
                |> Maybe.map (\c -> formatPrice parmac.priceDecimalDigits c.price)
                |> Maybe.withDefault ""
    in
    Model context index cycles parmac price False Array.empty


type Msg
    = ConfigNameChange String
    | ChangeType Int
    | PriceChange String
    | MoveTo Int
    | Remove
    | AskToRemove
    | DismissRemove
    | Copy
    | ExpandStep Int Bool
    | SelectParameter Int WMC.WashParameter


update : Msg -> Model -> ( Model, TabCmd )
update msg ({ cycles, index } as model) =
    case msg of
        ConfigNameChange name ->
            -- TODO: limita la dimensione a 32 caratteri
            ( { model | cycles = Array.Extra.update index (changeWashCycleName name model.context.language) cycles }, None )

        ChangeType selected ->
            ( { model | cycles = Array.Extra.update index (\c -> { c | washType = selected }) cycles }, None )

        PriceChange string ->
            ( { model | priceString = validateString model.parmac string |> Maybe.withDefault model.priceString }, None )

        MoveTo newIndex ->
            ( { model
                | cycles =
                    Maybe.map2 (\old new -> Array.set newIndex old cycles |> Array.set index new)
                        (Array.get index cycles)
                        (Array.get newIndex cycles)
                        |> Maybe.withDefault cycles
                , index = newIndex
              }
            , None
            )

        AskToRemove ->
            ( { model | removeDialog = True }, None )

        DismissRemove ->
            ( { model | removeDialog = False }, None )

        Remove ->
            ( { model | cycles = Array.Extra.removeAt index cycles }, Close )

        Copy ->
            ( { model
                | cycles =
                    Array.get index cycles
                        |> Maybe.map (\c -> Array.Extra.insertAt (index + 1) c cycles)
                        |> Maybe.withDefault cycles
              }
            , None
            )

        ExpandStep num bool ->
            ( { model
                | expandedSteps =
                    Array.Extra.resizelRepeat num False model.expandedSteps
                        |> Array.set num bool
              }
            , None
            )

        SelectParameter _ _ ->
            ( model, None )


validateString : MachineParameters -> String -> Maybe String
validateString parmac string =
    case string of
        "" ->
            Just ""

        value ->
            String.toFloat value
                |> Maybe.map (Round.round parmac.priceDecimalDigits)


view : Model -> Ui.Element Msg
view { index, cycles, parmac, context, priceString, removeDialog, expandedSteps } =
    let
        prevCycle =
            if index > 0 then
                index - 1

            else
                index

        nextCycle =
            if index < Array.length cycles - 1 then
                index + 1

            else
                index

        modals =
            (if removeDialog then
                [ Widget.dialog (Material.alertDialog Style.palette)
                    { title = Just <| translate Intl.Rimuovi context
                    , text = translate Intl.RimuovereIlLavaggio context
                    , accept = Just <| { text = translate Intl.Conferma context, onPress = Just Remove }
                    , dismiss = Just <| { text = translate Intl.Cancella context, onPress = Just DismissRemove }
                    }
                ]
                    |> Just

             else
                Nothing
            )
                |> Maybe.withDefault []
                |> Widget.singleModal

        priceLabel =
            Ui.text <| translate Intl.Prezzo context

        name =
            Array.get index cycles |> Maybe.map (\c -> getTranslation context.language c.name) |> Maybe.withDefault ""

        steps =
            Array.get index cycles |> Maybe.map .steps |> Maybe.withDefault Array.empty

        washType =
            Array.get index cycles |> Maybe.map .washType |> Maybe.withDefault 0
    in
    AppWidgets.scrollbarYEl (modals ++ [ Ui.width Ui.fill, Ui.height Ui.fill ]) <|
        Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacingXY 0 16 ]
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
                    [ Ui.el [ Font.size 64, Ui.centerY, Ui.alignLeft ] <| Ui.text <| String.fromInt (index + 1)
                    , Ui.column [ Ui.alignRight, Ui.centerY ]
                        [ AppWidgets.iconButton SolidIcons.arrowUp (MoveTo prevCycle) "up"
                        , AppWidgets.iconButton SolidIcons.arrowDown (MoveTo nextCycle) "down"
                        ]
                    , Ui.column [ Ui.centerY, Ui.alignRight ]
                        [ AppWidgets.iconButton SolidIcons.trash AskToRemove "remove"
                        , AppWidgets.iconButton SolidIcons.copy Copy "copy"
                        ]
                    ]
                ]
             , Ui.row [ Ui.width Ui.fill, Ui.spacing 32, Ui.paddingEach { top = 0, bottom = 0, left = 0, right = 32 } ]
                [ AppWidgets.washTypeChoice ChangeType (WMC.washTypesStrings context) washType |> Ui.el [ Ui.width Ui.fill, Ui.alignLeft ]
                , AppWidgets.washTypeImage washType
                ]
             , Style.br
             ]
                ++ (steps
                        |> Array.indexedMap (\i s -> AppWidgets.step context parmac ExpandStep SelectParameter (Array.get i expandedSteps |> Maybe.withDefault False) i s)
                        |> Array.toList
                   )
            )
