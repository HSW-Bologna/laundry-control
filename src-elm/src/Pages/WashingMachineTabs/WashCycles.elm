module Pages.WashingMachineTabs.WashCycles exposing (..)

import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font


type alias Model =
    { context : Context
    }


type Msg
    = NoMsg


view : Model -> Ui.Element Msg
view model =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
        [ Ui.paragraph [ Font.size 24 ] [ Ui.text (translate "lavaggi" model.context) ]
        ]
