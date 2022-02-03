module AppWidgets.Style exposing (..)

import Element as Ui
import Element.Border


border : List (Ui.Attribute msg)
border =
    [ Element.Border.color <| Ui.rgb255 200 200 200, Element.Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 } ]
