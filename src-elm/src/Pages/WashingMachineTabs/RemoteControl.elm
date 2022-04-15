module Pages.WashingMachineTabs.RemoteControl exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.IpAddress exposing (IpAddress)
import AppData.WashingMachineConfiguration as WMC
import AppData.WashingMachineState as WMS exposing (ConnectionState(..))
import AppWidgets.AppWidgets as AppWidgets
import Array exposing (Array)
import Bytes.Extra as Bytes
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import Ports as Ports


type alias SharedModel a =
    { a
        | connectionState : ConnectionState
        , context : Context
        , config : Maybe WMC.MachineConfiguration
    }


type Msg
    = LoadConfig String
    | SendConfig WMC.MachineConfiguration


update : Msg -> SharedModel a -> ( SharedModel a, Cmd msg )
update msg model =
    case msg of
        LoadConfig archive ->
            ( model, Ports.getRemoteMachineConfiguration archive )

        SendConfig archive ->
            ( model
            , Ports.sendRemoteMachineConfiguration
                archive.parmac.name
                (Array.fromList <| Bytes.toByteValues <| WMC.createArchive archive)
            )



-- VIEW


view : SharedModel a -> Ui.Element Msg
view ({ connectionState, context } as model) =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
        [ Ui.paragraph [ Font.size 24 ] [ Ui.text (translate Intl.ControlloRemoto context) ]
        , Ui.paragraph [ Ui.width Ui.fill ]
            [ case connectionState of
                Connected state metadata ->
                    machineView model state metadata

                Error error ->
                    Ui.text <| "Errore: " ++ error

                Disconnected ->
                    Ui.text <| "Disconnesso"
            ]
        ]


machineView : SharedModel a -> WMS.WashingMachineState -> WMS.Metadata -> Ui.Element Msg
machineView ({ context } as model) { state } metadata =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill ]
        [ case state of
            WMS.Running ->
                AppWidgets.textButton (translate Intl.Ferma context) Nothing

            WMS.Stopped ->
                AppWidgets.textButton (translate Intl.Esegui context) Nothing
        , metadataView model metadata
        ]


metadataView : SharedModel a -> WMS.Metadata -> Ui.Element Msg
metadataView { context, config } { machines } =
    let
        sendConfigMsg =
            Maybe.map SendConfig config
    in
    Ui.column []
        [ AppWidgets.archiveList machines LoadConfig
        , AppWidgets.textButton (translate Intl.InviaConfigurazione context) sendConfigMsg
        ]
