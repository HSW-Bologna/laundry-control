module Pages.WashingMachineTabs.RemoteControl exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.IpAddress exposing (IpAddress)
import AppData.WashingMachineState as WMS exposing (ConnectionState(..))
import AppWidgets.AppWidgets as AppWidgets
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



-- VIEW


view : SharedModel a -> Ui.Element Msg
view { connectionState, context } =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
        [ Ui.paragraph [ Font.size 24 ] [ Ui.text (translate Intl.ControlloRemoto context) ]
        , Ui.paragraph [ Ui.width Ui.fill ]
            [ case connectionState of
                Connected state ->
                    stateView context state

                Error error ->
                    Ui.text <| "Errore: " ++ error

                Disconnected ->
                    Ui.text <| "Disconnesso"
            ]
        ]


stateView : Context -> WMS.WashingMachineState -> Ui.Element msg
stateView context { state } =
    case state of
        WMS.Running ->
            AppWidgets.textButton (translate Intl.Ferma context) Nothing

        WMS.Stopped ->
            AppWidgets.textButton (translate Intl.Esegui context) Nothing
