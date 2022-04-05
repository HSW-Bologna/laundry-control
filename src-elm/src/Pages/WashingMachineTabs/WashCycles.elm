module Pages.WashingMachineTabs.WashCycles exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (getTranslation)
import AppData.Parameter exposing (formatPrice)
import AppData.WashingMachineConfiguration as WMC exposing (MachineParameters, WashingCycle, changeWashCycleName)
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
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


type alias Model =
    { context : Context
    , index : Int
    , cycle : WashingCycle
    , parmac : MachineParameters
    , priceString : String
    }


buildModel : Context -> Int -> WashingCycle -> MachineParameters -> Model
buildModel context index cycle parmac =
    Model context index cycle parmac <| formatPrice parmac.priceDecimalDigits cycle.price


type Msg
    = ConfigNameChange String
    | ChangeType Int
    | PriceChange String


update : Msg -> Model -> Model
update msg ({ cycle } as model) =
    case msg of
        ConfigNameChange name ->
            -- TODO: limita la dimensione a 32 caratteri
            { model | cycle = changeWashCycleName name model.context.language cycle }

        ChangeType selected ->
            { model | cycle = { cycle | washType = selected } }

        PriceChange string ->
            { model | priceString = validateString model.parmac string |> Maybe.withDefault model.priceString }


validateString : MachineParameters -> String -> Maybe String
validateString parmac string =
    case string of
        "" ->
            Just ""

        value ->
            String.toFloat value
                |> Maybe.map (Round.round parmac.priceDecimalDigits)


view : Model -> Ui.Element Msg
view model =
    let
        modals =
            Nothing
                |> Maybe.withDefault []
                |> Widget.singleModal

        priceLabel =
            Ui.text <| translate Intl.Prezzo model.context
    in
    AppWidgets.scrollbarYEl (modals ++ [ Ui.width Ui.fill, Ui.height Ui.fill ]) <|
        Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacingXY 0 16 ]
            [ Ui.row [ Ui.width Ui.fill, Ui.spacing 32 ]
                [ Input.text [ Ui.alignLeft, Ui.width Ui.fill ]
                    { onChange = ConfigNameChange
                    , text = getTranslation model.context.language model.cycle.name
                    , placeholder = Nothing
                    , label = Input.labelHidden "name"
                    }
                , Ui.row []
                    [ Ui.el [ Font.size 64, Ui.centerY, Ui.alignLeft ] <| Ui.text <| String.fromInt (model.index + 1)
                    , Ui.column [ Ui.alignRight, Ui.centerY ]
                        [ Widget.iconButton (Material.iconButton Material.defaultPalette)
                            { text = "up"
                            , icon = SolidIcons.arrowUp |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
                            , onPress = Nothing
                            }
                        , Widget.iconButton (Material.iconButton Material.defaultPalette)
                            { text = "down"
                            , icon = SolidIcons.arrowDown |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
                            , onPress = Nothing
                            }
                        ]
                    ]
                ]
            , Ui.row [ Ui.width Ui.fill, Ui.spacing 32, Ui.paddingEach { top = 0, bottom = 0, left = 0, right = 32 } ]
                [ AppWidgets.washTypeChoice ChangeType (WMC.washTypesStrings model.context) model.cycle.washType |> Ui.el [ Ui.width Ui.fill, Ui.alignLeft ]
                , AppWidgets.washTypeImage model.cycle.washType
                ]
            , Style.br
            , Input.text [ Ui.width Ui.fill ]
                { onChange = PriceChange
                , text = model.priceString
                , placeholder = Just <| Input.placeholder [] priceLabel
                , label = Input.labelLeft [] priceLabel
                }
            ]
