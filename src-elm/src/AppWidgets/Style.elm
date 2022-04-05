module AppWidgets.Style exposing (..)

import Element as Ui
import Element.Border


border : List (Ui.Attribute msg)
border =
    [ Element.Border.color <| Ui.rgb255 200 200 200, Element.Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 } ]


br : Ui.Element msg
br =
    Ui.el [ Ui.centerX, Ui.width Ui.fill, Ui.paddingXY 32 0 ] <|
        Ui.el [ Ui.centerX, borderColor, Element.Border.width 1, Ui.height (Ui.px 0), Ui.width Ui.fill ] Ui.none


borderColor =
    Element.Border.color (Ui.rgb255 210 210 210)

