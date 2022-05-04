module AppWidgets.AppWidgets exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (IntlString, Language(..), languageString)
import AppData.IpAddress as IpAddress exposing (IpAddress, asList, changePart)
import AppData.Parameter as Parameter exposing (Parameter)
import AppData.WashingMachineConfiguration as WMC exposing (WashingStep)
import AppWidgets.Style as Style
import Array
import Context exposing (Context, translate)
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Attributes
import FontAwesome.Icon as FAIcon
import FontAwesome.Solid as SolidIcons
import FontAwesome.Svg as FontAwesomeSvg
import Html
import Html.Attributes as Attributes
import Html.Events
import Widget as Widget
import Widget.Icon as Icon
import Widget.Material as Material



-- PARAMETER VIEW


parameter : db -> info -> Context -> (Parameter db info -> msg) -> Int -> Parameter db info -> Ui.Element msg
parameter db parmac context msg _ par =
    Ui.column [ Ui.width Ui.fill, Ui.spacing 8 ]
        [ Ui.row
            [ Ui.width Ui.fill
            , Ui.spacing 4
            , Ui.paddingXY 8 16
            , Border.rounded 4
            , Events.onClick (msg par)
            , Ui.mouseOver [ Background.color <| Ui.rgb 0.9 0.9 0.9 ]
            , Background.color <| Ui.rgb 1 1 1
            ]
            [ ("   " ++ translate par.description context)
                |> Ui.text
                |> Ui.el [ Ui.alignLeft ]
            , par.format parmac context (par.get db)
                |> Ui.text
                |> Ui.el [ Ui.alignRight ]
            ]
        , Style.br
        ]


type ParameterModificationData
    = NumberParameter String
    | OptionParameter String
    | PriceParameter String


parameterModificationData : db -> info -> Context -> Parameter db info -> ParameterModificationData
parameterModificationData db info context ({ ui, get, format } as par) =
    case ui of
        Parameter.Number ->
            NumberParameter (String.fromInt <| get db)

        Parameter.Option ->
            OptionParameter (Parameter.indexToOption info context par <| get db)

        Parameter.Price ->
            PriceParameter (format info context <| get db)


parameterModificationValue : Parameter db info -> info -> Context -> ParameterModificationData -> Int -> Maybe Int
parameterModificationValue par b context data priceMultiplier =
    case data of
        NumberParameter string ->
            String.toInt string

        OptionParameter string ->
            Parameter.optionToIndex b context par string

        PriceParameter string ->
            String.toFloat string |> Maybe.map ((*) (toFloat (10 ^ priceMultiplier)) >> round)


validateParameterInput : String -> ParameterModificationData -> ParameterModificationData
validateParameterInput text data =
    let
        validateEditString : String -> Maybe String
        validateEditString string =
            case string of
                "" ->
                    Just ""

                value ->
                    String.toInt value
                        |> Maybe.map (\_ -> value)
    in
    case data of
        NumberParameter _ ->
            validateEditString text
                |> Maybe.map NumberParameter
                |> Maybe.withDefault data

        OptionParameter _ ->
            OptionParameter text

        PriceParameter _ ->
            String.toFloat text
                |> Maybe.map (always (PriceParameter text))
                |> Maybe.withDefault data


parameterModificationDialog :
    { info : info
    , context : Context
    , textChange : String -> msg
    , dismiss : msg
    , confirm : Parameter db info -> Int -> msg
    , modData : ParameterModificationData
    , par : Parameter db info
    , priceMultiplier : Int
    }
    -> Ui.Element msg
parameterModificationDialog { info, context, textChange, dismiss, confirm, modData, par, priceMultiplier } =
    let
        validationResult =
            parameterModificationValue par info context modData priceMultiplier
                |> Maybe.map (Parameter.validate par)
                |> Maybe.withDefault (Err Intl.ValoreNonNumerico)

        input =
            let
                textInput text =
                    Input.text
                        [ Ui.width Ui.fill ]
                        { onChange = textChange, text = text, placeholder = Nothing, label = Input.labelLeft [] (Ui.text (translate Intl.Valore context)) }
            in
            case modData of
                NumberParameter text ->
                    textInput text

                OptionParameter text ->
                    Ui.el [ Ui.alignRight ]
                        (List.map
                            (\opt ->
                                Html.option
                                    [ Attributes.selected (text == opt) ]
                                    [ Html.text opt ]
                            )
                            (Parameter.options info context par)
                            |> Html.select [ Attributes.style "font-size" "inherit", Html.Events.onInput textChange ]
                            |> Ui.html
                        )

                PriceParameter text ->
                    textInput text
    in
    Ui.column (Style.modal 400)
        [ Ui.paragraph [ Ui.width Ui.fill ]
            [ translate par.description context |> Ui.text ]
        , input
        , Ui.paragraph [ Ui.width Ui.fill, Font.color (Ui.rgb 1 0.1 0.1), Ui.height <| Ui.px 48 ]
            [ case validationResult of
                Ok _ ->
                    Ui.none

                Err error ->
                    Ui.text <| translate error context
            ]
        , Ui.row [ Ui.width Ui.fill ]
            [ textButton (translate Intl.Cancella context) (Just dismiss)
            , textButton
                (translate Intl.Conferma context)
                (validationResult |> Result.toMaybe |> Maybe.map (confirm par))
                |> Ui.el [ Ui.alignRight ]
            ]
        ]


step : Context -> WMC.MachineParameters -> (Int -> Bool -> msg) -> (Int -> WMC.WashParameter -> msg) -> Bool -> Int -> WashingStep -> Ui.Element msg
step context parmac toggle selected expanded index s =
    Widget.expansionItem (Material.expansionItem Style.palette)
        { onToggle = toggle index
        , isExpanded = expanded
        , icon = always Ui.none
        , text = String.fromInt (index + 1) ++ " - " ++ WMC.washStepTypeToString context s.stepType
        , content =
            WMC.stepParameterMetadataList s.stepType parmac
                |> List.indexedMap
                    (parameter s parmac context (selected index))
                |> List.map Widget.asItem
        }
        |> Widget.itemList (Material.cardColumn Style.palette)



-- GLOBAL WIDGETS


languageSelect : Context -> (String -> msg) -> Ui.Element msg
languageSelect context msg =
    Ui.html <|
        Html.select [ Attributes.style "font-size" "inherit", Html.Events.onInput msg ] <|
            List.map
                (\l ->
                    Html.option
                        [ Attributes.selected (l == context.language)
                        ]
                        [ Html.text <| languageString l ]
                )
                [ Italiano, English ]


type alias DrawerConfig msg =
    { context : Context
    , selected : Maybe Int
    , back : msg
    , goToRemoteControl : msg
    , goToConfig : Maybe msg
    , selectedCycle : Maybe Int
    , cyclesExpanded : Bool
    , toggleCycles : Bool -> msg
    , cycles : List String
    , goToCycle : Int -> msg
    }


leftDrawer : DrawerConfig msg -> Ui.Element msg
leftDrawer { context, selected, back, goToConfig, cyclesExpanded, toggleCycles, cycles, selectedCycle, goToCycle, goToRemoteControl } =
    let
        tabOption ( name, msg ) =
            { text = translate name context
            , icon = always Ui.none
            }

        mainOptions =
            [ ( Intl.ControlloRemoto, Just goToRemoteControl )
            , ( Intl.ParametriMacchina, goToConfig )
            ]
    in
    (Widget.fullBleedItem (Material.fullBleedItem Style.palette)
        { onPress = Just back
        , icon = SolidIcons.home |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
        , text = translate Intl.Indietro context
        }
        :: ({ selected = selected
            , options = List.map tabOption mainOptions
            , onSelect = \s -> Maybe.andThen Tuple.second <| Array.get s <| Array.fromList mainOptions
            }
                |> Widget.selectItem (Material.selectItem Style.palette)
           )
        ++ Widget.expansionItem (Material.expansionItem Style.palette)
            { onToggle = toggleCycles
            , isExpanded = cyclesExpanded
            , icon = always Ui.none
            , text = translate Intl.Cicli context
            , content =
                cyclesItems context selectedCycle goToCycle cycles
            }
    )
        |> Widget.itemList (Material.cardColumn Style.palette)
        |> scrollbarYEl [ Ui.height Ui.fill, Ui.width <| Ui.px 346, Ui.clipX ]
        |> Ui.el ([ Ui.alignLeft, Ui.height Ui.fill, Ui.width <| Ui.px 346, Ui.clipX ] ++ Style.border)


cyclesItems : Context -> Maybe Int -> (Int -> msg) -> List String -> List (Widget.Item msg)
cyclesItems context selected select cycles =
    Widget.selectItem (Material.selectItem Style.palette)
        { selected = selected
        , options =
            List.map (\name -> { text = name, icon = always Ui.none }) cycles
                ++ (if List.length cycles < 100 then
                        [ { text = translate Intl.NuovoProgramma context, icon = SolidIcons.plus |> Icon.elmFontawesome FontAwesomeSvg.viewIcon } ]

                    else
                        []
                   )
        , onSelect = \i -> Just <| select i
        }


rightMenu : Context -> List ( IntlString, Maybe msg ) -> Ui.Element msg
rightMenu context options =
    let
        menuOption ( name, msg ) =
            Widget.fullBleedItem (Material.fullBleedItem Style.palette)
                { onPress = msg
                , icon = always Ui.none
                , text = translate name context
                }
    in
    List.map menuOption options
        |> Widget.itemList (Material.cardColumn Style.palette)
        |> Ui.el (Ui.alignRight :: Style.border)


ipDialog : Context -> Maybe (List ( IpAddress, String )) -> IpAddress -> (IpAddress -> msg) -> (Maybe IpAddress -> msg) -> msg -> List (Ui.Attribute msg)
ipDialog context available ip msg submit refresh =
    let
        button text event align =
            Ui.el [ align ] <|
                Widget.textButton (Material.textButton Style.palette) { text = text, onPress = Just event }
    in
    { onDismiss = Just <| submit Nothing
    , content =
        Ui.el
            (Style.modal 480)
            (Ui.column [ Ui.height Ui.fill, Ui.width Ui.fill, Ui.spacing 16 ]
                [ Ui.text <|
                    translate Intl.InserisciIp
                        context
                , Ui.row [ Ui.centerX, Ui.spacing 2 ] <|
                    List.intersperse (Ui.text ".") <|
                        List.indexedMap
                            (\i val ->
                                Input.text []
                                    { onChange = \s -> msg <| changePart ip i (Maybe.withDefault 0 <| String.toInt s)
                                    , text = String.fromInt val
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "ip"
                                    }
                            )
                            (asList ip)
                , case available of
                    Nothing ->
                        Ui.el [ Ui.centerX ] <|
                            Widget.circularProgressIndicator (Material.progressIndicator Style.palette) Nothing

                    Just availableList ->
                        availableList
                            |> List.map
                                (\( x, node ) ->
                                    Widget.fullBleedItem (Material.fullBleedItem Style.palette)
                                        { onPress = Just <| msg x
                                        , icon = always Ui.none
                                        , text = IpAddress.toString x ++ " : " ++ node
                                        }
                                )
                            |> Widget.itemList (Material.cardColumn Style.palette)
                            |> Ui.el [ Ui.height <| Ui.maximum 240 Ui.fill, Ui.width Ui.fill ]
                , Ui.row
                    [ Ui.alignBottom, Ui.width Ui.fill ]
                    [ button "cancella" (submit Nothing) Ui.alignLeft
                    , iconButton SolidIcons.recycle refresh "refresh" |> Ui.el [ Ui.centerX ]
                    , button "conferma" (submit <| Just ip) Ui.alignRight
                    ]
                ]
            )
    }
        |> List.singleton
        |> Widget.singleModal


washTypeImage : Int -> Int -> Ui.Element msg
washTypeImage washType width =
    [ "molto_sporchi_con_prelavaggio_inox"
    , "sporchi_con_prelavaggio_inox"
    , "molto_sporchi_inox"
    , "sporchi_inox"
    , "colorati_inox"
    , "sintetici_inox"
    , "piumoni_inox"
    , "freddo_inox"
    , "lana_inox"
    , "fibre_naturali_inox"
    , "solo_centrifuga_inox"
    , "solo_centrifuga_inox"
    , "igienizza_cesto_inox"
    , "ammollo_inox"
    , "prelavaggio_centrifuga_inox"
    , "risciacquo_centrifuga_inox"
    ]
        |> List.map (\s -> "images/" ++ s ++ ".png")
        |> Array.fromList
        |> Array.get washType
        |> Maybe.withDefault ""
        |> (\src ->
                Ui.image
                    [ Ui.alignRight, Ui.width <| Ui.px width ]
                    { src = src, description = "Wash type" }
           )



-- GENERIC WIDGETS


scrollbarYEl : List (Ui.Attribute msg) -> Ui.Element msg -> Ui.Element msg
scrollbarYEl attrs body =
    Ui.el [ Ui.height Ui.fill, Ui.width Ui.fill, Ui.scrollbarY ] <|
        Ui.el
            ([ Ui.htmlAttribute <| Attributes.style "position" "absolute"
             , Ui.htmlAttribute <| Attributes.style "top" "0"
             , Ui.htmlAttribute <| Attributes.style "right" "0"
             , Ui.htmlAttribute <| Attributes.style "bottom" "0"
             , Ui.htmlAttribute <| Attributes.style "left" "0"
             , Ui.scrollbarY
             ]
                ++ attrs
            )
            body


textButton : String -> Maybe msg -> Ui.Element msg
textButton text msg =
    Widget.textButton (Material.textButton Style.palette)
        { text = text
        , onPress = msg
        }


stringChoice : (Int -> msg) -> List String -> Int -> Ui.Element msg
stringChoice select options selected =
    Widget.select
        { selected = Just selected
        , options =
            options
                |> List.map
                    (\text ->
                        { text = text
                        , icon = always Ui.none
                        }
                    )
        , onSelect = \i -> Just (select i)
        }
        |> Widget.wrappedButtonRow
            { elementRow = Material.row
            , content = Material.outlinedButton Style.palette
            }


iconButton : FAIcon.Icon -> msg -> String -> Ui.Element msg
iconButton icon msg text =
    Widget.iconButton
        (Material.iconButton Style.palette)
        { text = text
        , icon = icon |> Icon.elmFontawesome (FAIcon.viewStyled [ FontAwesome.Attributes.lg ])
        , onPress = Just <| msg
        }


homeButton : Context -> msg -> Ui.Element msg
homeButton context msg =
    Widget.iconButton (Material.iconButton Style.palette)
        { text = translate Intl.Indietro context
        , icon = SolidIcons.home |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
        , onPress = Just msg
        }
