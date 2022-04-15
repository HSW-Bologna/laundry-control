module AppWidgets.Style exposing (..)

import Color as Color
import Element as Ui
import Element.Border
import Framework.Card as Card
import Widget.Material as Material


border : List (Ui.Attribute msg)
border =
    [ Element.Border.color <| Ui.rgb255 200 200 200, Element.Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 } ]


br : Ui.Element msg
br =
    Ui.el [ Ui.centerX, Ui.width Ui.fill, Ui.paddingXY 32 0 ] <|
        Ui.el [ Ui.centerX, borderColor, Element.Border.width 1, Ui.height (Ui.px 0), Ui.width Ui.fill ] Ui.none


borderColor : Ui.Attr decorative msg
borderColor =
    Element.Border.color (Ui.rgb255 0x60 0x7D 0x8B)


focusedBorder : Bool -> List (Ui.Attr () msg)
focusedBorder focused =
    if focused then
        [ borderColor, Element.Border.width 2, Element.Border.rounded 4 ]

    else
        []


palette : Material.Palette
palette =
    { primary = Color.rgb255 0x21 0x96 0xF3
    , secondary = Color.rgb255 0x60 0x7D 0x8B
    , background = Color.rgb255 0xFF 0xFF 0xFF
    , surface = Color.rgb255 0xFF 0xFF 0xFF
    , error = Color.rgb255 0xB0 0x00 0x20
    , on =
        { primary = Color.rgb255 0xFF 0xFF 0xFF
        , secondary = Color.rgb255 0x00 0x00 0x00
        , background = Color.rgb255 0x00 0x00 0x00
        , surface = Color.rgb255 0x00 0x00 0x00
        , error = Color.rgb255 0xFF 0xFF 0xFF
        }
    }


modal : Int -> List (Ui.Attribute msg)
modal width =
    Card.simple ++ [ Ui.spacing 16, Ui.centerX, Ui.centerY, Ui.padding 32, Ui.width <| Ui.px width ]
