module AppWidgets.AppWidgets exposing (..)

import AppWidgets.Style as Style
import Array
import Context exposing (Context, translate)
import Element as Ui
import FontAwesome.Solid as SolidIcons
import FontAwesome.Svg as FontAwesomeSvg
import Html
import Html.Attributes as Attributes
import Html.Events
import Intl exposing (Language(..), languageString)
import Widget as Widget
import Widget.Icon as Icon
import Widget.Material as Material


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
                [ Italian, English ]


leftDrawer : Context -> msg -> Maybe Int -> List ( String, Maybe msg ) -> Ui.Element msg
leftDrawer context back selected options =
    let
        tabOption ( name, msg ) =
            { text = translate name context
            , icon = always Ui.none
            }
    in
    (Widget.fullBleedItem (Material.fullBleedItem Material.defaultPalette)
        { onPress = Just back
        , icon = SolidIcons.home |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
        , text = translate "indietro" context
        }
        :: ({ selected = selected
            , options = List.map tabOption options
            , onSelect = \s -> Maybe.andThen Tuple.second <| Array.get s <| Array.fromList options
            }
                |> Widget.selectItem (Material.selectItem Material.defaultPalette)
           )
    )
        |> Widget.itemList (Material.cardColumn Material.defaultPalette)
        |> Ui.el ([ Ui.alignLeft, Ui.height Ui.fill ] ++ Style.border)


rightMenu : Context -> List ( String, Maybe msg ) -> Ui.Element msg
rightMenu context options =
    let
        menuOption ( name, msg ) =
            Widget.fullBleedItem (Material.fullBleedItem Material.defaultPalette)
                { onPress = msg
                , icon = always Ui.none
                , text = translate name context
                }
    in
    List.map menuOption options
        |> Widget.itemList (Material.cardColumn Material.defaultPalette)
        |> Ui.el (Ui.alignRight :: Style.border)


homeButton : Context -> msg -> Ui.Element msg
homeButton context msg =
    Widget.iconButton (Material.iconButton Material.defaultPalette)
        { text = translate "indietro" context
        , icon = SolidIcons.home |> Icon.elmFontawesome FontAwesomeSvg.viewIcon
        , onPress = Just msg
        }
