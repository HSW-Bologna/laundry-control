module Pages.WashingMachineTabs.RemoteControl exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.IpAddress exposing (IpAddress)
import AppData.WashingMachineState exposing (ConnectionState(..))
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font


type alias SharedModel a =
    { a
        | connectionState : ConnectionState
        , context : Context
    }


type Msg
    = ConnectTo IpAddress


update : Msg -> SharedModel a -> ( SharedModel a, Cmd Msg )
update msg model =
    case msg of
        ConnectTo addr ->
            ( model, Cmd.none )


view : SharedModel a -> Ui.Element Msg
view { connectionState, context } =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
        [ Ui.paragraph [ Font.size 24 ] [ Ui.text (translate Intl.ControlloRemoto context) ]
        , Ui.text <|
            case connectionState of
                Connected _ ->
                    "Connesso"

                Error error ->
                    "Errore: " ++ error

                Disconnected ->
                    "Disconnesso"
        ]
