module AppWidgets.AppWidgets exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (IntlString, Language(..), languageString)
import AppData.IpAddress exposing (IpAddress, asList)
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
import FontAwesome.Icon as FAIcon
import FontAwesome.Solid as SolidIcons
import FontAwesome.Svg as FontAwesomeSvg
import Framework.Card as Card
import Html
import Html.Attributes as Attributes
import Html.Events
import Widget as Widget
import Widget.Icon as Icon
import Widget.Material as Material


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


parameter : Context -> a -> b -> (Parameter a b -> msg) -> Parameter a b -> Ui.Element msg
parameter context db parmac msg par =
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
            [ translate par.description context
                |> Ui.text
                |> Ui.el [ Ui.alignLeft ]
            , par.format parmac context (par.get db)
                |> Ui.text
                |> Ui.el [ Ui.alignRight ]
            ]
        , Style.br
        ]


parameterModificationDialog : b -> Context -> (Parameter a b -> String -> msg) -> msg -> (Parameter a b -> Int -> msg) -> String -> Parameter a b -> Ui.Element msg
parameterModificationDialog b context textChange dismiss confirm text par =
    let
        validationResult =
            Parameter.validate text par

        input =
            case par.ui of
                Parameter.Number ->
                    Input.text
                        [ Ui.width Ui.fill ]
                        { onChange = textChange par, text = text, placeholder = Nothing, label = Input.labelLeft [] (Ui.text (translate Intl.Valore context)) }

                Parameter.Option ->
                    Ui.el [ Ui.alignRight ]
                        (List.map
                            (\opt ->
                                Html.option
                                    [ Attributes.selected (text == opt) ]
                                    [ Html.text opt ]
                            )
                            (Parameter.options b context par)
                            |> Html.select [ Attributes.style "font-size" "inherit", Html.Events.onInput (textChange par) ]
                            |> Ui.html
                        )
    in
    Ui.column (Card.simple ++ [ Ui.spacing 16, Ui.centerX, Ui.centerY, Ui.padding 32, Ui.width <| Ui.px 380 ])
        [ Ui.paragraph [ Ui.width Ui.fill ]
            [ translate par.description context |> Ui.text ]
        , input
        , Ui.paragraph [ Ui.width Ui.fill, Font.color (Ui.rgb 1 0.1 0.1), Ui.height <| Ui.px 48 ]
            [ case validationResult of
                Ok () ->
                    Ui.none

                Err error ->
                    Ui.text <| translate error context
            ]
        , Ui.row [ Ui.width Ui.fill ]
            [ textButton (translate Intl.Cancella context) (Just dismiss)
            , textButton
                (translate Intl.Conferma context)
                (validationResult |> Result.toMaybe |> Maybe.map (\_ -> confirm par <| Parameter.fromString b context par text))
                |> Ui.el [ Ui.alignRight ]
            ]
        ]


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
        |> Ui.el ([ Ui.alignLeft, Ui.height Ui.fill ] ++ Style.border)


cyclesItems : Context -> Maybe Int -> (Int -> msg) -> List String -> List (Widget.Item msg)
cyclesItems context selected select cycles =
    Widget.selectItem (Material.selectItem Style.palette)
        { selected = selected
        , options =
            List.map (\name -> { text = name, icon = always Ui.none }) cycles
                ++ [ { text = translate Intl.NuovoProgramma context, icon = SolidIcons.plus |> Icon.elmFontawesome FontAwesomeSvg.viewIcon } ]
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


homeButton : Context -> msg -> Ui.Element msg
homeButton context msg =
    Widget.iconButton (Material.iconButton Style.palette)
        { text = translate Intl.Indietro context
        , icon = SolidIcons.home |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
        , onPress = Just msg
        }


ipDialog : Context -> IpAddress -> (Int -> Int -> msg) -> (Maybe IpAddress -> msg) -> List (Ui.Attribute msg)
ipDialog context ip msg submit =
    let
        button text event align =
            Ui.el [ align ] <|
                Widget.textButton (Material.textButton Style.palette) { text = text, onPress = Just event }
    in
    { onDismiss = Just <| submit Nothing
    , content =
        Ui.el
            (Card.simple
                ++ [ Ui.width <| Ui.px 320, Ui.height <| Ui.px 240, Ui.centerX, Ui.centerY ]
            )
            (Ui.column [ Ui.height Ui.fill, Ui.width Ui.fill ]
                [ Ui.text <|
                    translate Intl.InserisciIp
                        context
                , Ui.row [ Ui.centerX, Ui.spacing 2 ] <|
                    List.intersperse (Ui.text ".") <|
                        List.indexedMap
                            (\i val ->
                                Input.text []
                                    { onChange = \s -> msg i (Maybe.withDefault 0 <| String.toInt s)
                                    , text = String.fromInt val
                                    , placeholder = Nothing
                                    , label = Input.labelHidden "ip"
                                    }
                            )
                            (asList ip)
                , Ui.row
                    [ Ui.alignBottom, Ui.width Ui.fill ]
                    [ button "cancella" (submit Nothing) Ui.alignLeft
                    , button "conferma" (submit <| Just ip) Ui.alignLeft
                    ]
                ]
            )
    }
        |> List.singleton
        |> Widget.singleModal


scrollbarYEl : List (Ui.Attribute msg) -> Ui.Element msg -> Ui.Element msg
scrollbarYEl attrs body =
    Ui.el [ Ui.height Ui.fill, Ui.width Ui.fill ] <|
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


washTypeChoice : (Int -> msg) -> List String -> Int -> Ui.Element msg
washTypeChoice select options selected =
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


washTypeImage : Int -> Ui.Element msg
washTypeImage washType =
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
                    [ Ui.alignRight, Ui.width <| Ui.px 160 ]
                    { src = src, description = "Wash type" }
           )


iconButton : FAIcon.Icon -> msg -> String -> Ui.Element msg
iconButton icon msg text =
    Widget.iconButton
        (Material.iconButton Style.palette)
        { text = text
        , icon = icon |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
        , onPress = Just <| msg
        }


step : Context -> WMC.MachineParameters -> (Int -> Bool -> msg) -> (Int -> WMC.WashParameter -> msg) -> Bool -> Int -> WashingStep -> Ui.Element msg
step context parmac toggle selected expanded index s =
    Widget.expansionItem (Material.expansionItem Style.palette)
        { onToggle = toggle index
        , isExpanded = expanded
        , icon = always Ui.none
        , text = String.fromInt index ++ " " ++ WMC.washStepTypeToString context s.stepType
        , content =
            WMC.stepParameterMetadataList s.stepType
                |> List.map
                    (parameter context s parmac (selected index))
                |> List.map Widget.asItem
        }
        |> Widget.itemList (Material.cardColumn Style.palette)
