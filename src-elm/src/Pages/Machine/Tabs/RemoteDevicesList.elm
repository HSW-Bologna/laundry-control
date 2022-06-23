module Pages.Machine.Tabs.RemoteDevicesList exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.Things5 exposing (Device)
import AppWidgets.AppWidgets as AppWidgets
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import Element.Input as Input
import Ports


type alias SharedModel a =
    { a
        | context : Context
        , things5Token : Maybe String
        , things5Devices : List Device
    }


type alias Model =
    { username : String
    , password : String
    }


buildModel : Model
buildModel =
    Model "" ""


type Msg
    = MsgUsernameInput String
    | MsgPasswordInput String
    | MsgLoginButton
    | MsgRefreshButton
    | MsgConnectButton String String
    | MsgLogout


update : Msg -> SharedModel a -> Model -> ( SharedModel a, Model, Cmd msg )
update msg sharedModel model =
    case msg of
        MsgUsernameInput string ->
            ( sharedModel, { model | username = string }, Cmd.none )

        MsgPasswordInput string ->
            ( sharedModel, { model | password = string }, Cmd.none )

        MsgLoginButton ->
            ( sharedModel, model, Ports.things5Login model.username model.password )

        MsgConnectButton token id ->
            ( sharedModel, model, Ports.washingMachineThings5Connect token id )

        MsgRefreshButton ->
            ( sharedModel, model, Cmd.none )

        MsgLogout ->
            ( { sharedModel | things5Token = Nothing }, model, Cmd.none )



-- VIEW


view : SharedModel a -> Model -> Ui.Element Msg
view { context, things5Token, things5Devices } { username, password } =
    Ui.column
        [ Ui.width Ui.fill
        , Ui.height Ui.fill
        , Ui.padding 16
        , Ui.spacing 16
        ]
        [ Ui.paragraph [ Font.size 32, Ui.centerX, Ui.width Ui.fill ] [ Ui.text (translate Intl.DispositiviRemoti context) ]
        , case things5Token of
            Nothing ->
                Ui.column [ Ui.centerX, Ui.centerY, Ui.spacing 32 ]
                    [ Input.text [ Ui.width <| Ui.px 320 ]
                        { onChange = MsgUsernameInput
                        , text = username
                        , placeholder = Just <| Input.placeholder [] (Ui.text <| translate Intl.Utente context)
                        , label = Input.labelLeft [] (Ui.el [ Ui.width <| Ui.px 140 ] <| Ui.text <| translate Intl.Utente context)
                        }
                    , Input.currentPassword [ Ui.width <| Ui.px 320 ]
                        { onChange = MsgPasswordInput
                        , text = password
                        , placeholder = Just <| Input.placeholder [] (Ui.text <| translate Intl.Password context)
                        , label = Input.labelLeft [] (Ui.el [ Ui.width <| Ui.px 140 ] <| Ui.text <| translate Intl.Password context)
                        , show = False
                        }
                    , AppWidgets.textButton
                        (translate Intl.Conferma context)
                        (if username == "" || password == "" then
                            Nothing

                         else
                            Just MsgLoginButton
                        )
                        |> Ui.el [ Ui.centerX ]
                    ]

            Just token ->
                Ui.el
                    [ Ui.width Ui.fill
                    , Ui.height Ui.fill
                    , Ui.inFront <| Ui.el [ Ui.alignTop, Ui.alignRight ] <| AppWidgets.textButton (translate Intl.Esci context) (Just MsgLogout)
                    , Ui.inFront <| Ui.el [ Ui.alignBottom, Ui.alignRight ] <| AppWidgets.textButton (translate Intl.Aggiorna context) (Just MsgRefreshButton)
                    ]
                <|
                    Ui.wrappedRow [ Ui.centerX, Ui.spacing 32, Ui.padding 64 ] <|
                        List.map (\d -> AppWidgets.machineSelectionButton (MsgConnectButton token d.id) d.name) things5Devices
        ]
        |> AppWidgets.scrollbarYEl [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
