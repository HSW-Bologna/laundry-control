module Pages.Machine.Tabs.RemoteDevicesList exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.Things5 exposing (Device)
import AppWidgets.AppWidgets as AppWidgets
import Context exposing (Context, translate)
import Element as Ui
import Element.Background as Background
import Element.Border as Border
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
    = UsernameInput String
    | PasswordInput String
    | LoginButton
    | RefreshButton
    | ConnectButton String


update : Msg -> SharedModel a -> Model -> ( SharedModel a, Model, Cmd msg )
update msg sharedModel model =
    case msg of
        UsernameInput string ->
            ( sharedModel, { model | username = string }, Cmd.none )

        PasswordInput string ->
            ( sharedModel, { model | password = string }, Cmd.none )

        LoginButton ->
            ( sharedModel, model, Ports.things5Login model.username model.password )

        ConnectButton id ->
            ( sharedModel, model, Cmd.none )

        RefreshButton ->
            ( sharedModel, model, Cmd.none )



-- VIEW


view : SharedModel a -> Model -> Ui.Element Msg
view { context, things5Token, things5Devices } { username, password } =
    let
        primaryColor =
            Ui.rgba255 0x62 0x00 0xEE 1.0

        primaryColorLight =
            Ui.rgba255 0x62 0x00 0xEE 0.8

        primaryColorLighter =
            Ui.rgba255 0x62 0x00 0xEE 0.4

        selectionButton text device =
            Input.button
                [ Ui.padding 48
                , Border.width 4
                , Border.color primaryColor
                , Border.rounded 16
                , Ui.mouseDown
                    [ Background.color primaryColorLighter
                    , Font.color <| Ui.rgb 0 0 0
                    ]
                , Ui.mouseOver [ Background.color primaryColorLight ]
                , Background.color primaryColor
                , Font.color <| Ui.rgb 1 1 1
                , Font.size 32
                ]
                { onPress = Just (ConnectButton device), label = Ui.text text }
    in
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacing 16 ]
        [ Ui.paragraph [ Font.size 32, Ui.centerX, Ui.width Ui.fill ] [ Ui.text (translate Intl.DispositiviRemoti context) ]
        , case things5Token of
            Nothing ->
                Ui.column [ Ui.centerX, Ui.centerY, Ui.spacing 32 ]
                    [ Input.text [ Ui.width <| Ui.px 240 ]
                        { onChange = UsernameInput
                        , text = username
                        , placeholder = Just <| Input.placeholder [] (Ui.text <| translate Intl.Utente context)
                        , label = Input.labelLeft [] (Ui.el [ Ui.width <| Ui.px 160 ] <| Ui.text <| translate Intl.Utente context)
                        }
                    , Input.currentPassword [ Ui.width <| Ui.px 240 ]
                        { onChange = PasswordInput
                        , text = password
                        , placeholder = Just <| Input.placeholder [] (Ui.text <| translate Intl.Password context)
                        , label = Input.labelLeft [] (Ui.el [ Ui.width <| Ui.px 160 ] <| Ui.text <| translate Intl.Password context)
                        , show = False
                        }
                    , AppWidgets.textButton
                        (translate Intl.Conferma context)
                        (if username == "" && password == "" then
                            Nothing

                         else
                            Just LoginButton
                        )
                        |> Ui.el [ Ui.centerX ]
                    ]

            Just token ->
                Ui.el
                    [ Ui.width Ui.fill
                    , Ui.height Ui.fill
                    , Ui.inFront <| Ui.el [ Ui.alignBottom, Ui.alignRight ] <| AppWidgets.textButton (translate Intl.Aggiorna context) (Just RefreshButton)
                    ]
                <|
                    Ui.wrappedRow [ Ui.centerX, Ui.spacing 32, Ui.padding 64 ] <|
                        List.map (\d -> selectionButton d.name d.id) things5Devices
        ]
        |> AppWidgets.scrollbarYEl [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
