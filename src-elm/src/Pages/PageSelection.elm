port module Pages.PageSelection exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (Language, languageFromString, languageString)
import AppWidgets.AppWidgets exposing (languageSelect)
import Browser
import Context exposing (Context, changeLanguage, translate)
import Element as Ui
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as UiIn
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (decodeEvent, navigateToPage, preferences)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { context : Context }


init : String -> ( Model, Cmd Msg )
init language =
    ( Model <| Context (languageFromString language), Cmd.none )



-- UPDATE


type Msg
    = PickMachine String
    | ChangeLanguage String
    | SavedPreferences Encode.Value


port savedPreferences : (Encode.Value -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SavedPreferences value ->
            case decodeEvent "savedPreferences" preferencesDecoder value of
                Ok { language, machine } ->
                    ( model, navigateToPage machine (languageFromString language) )

                _ ->
                    ( model, Cmd.none )

        PickMachine machine ->
            ( model, Cmd.batch [ preferences { machine = machine, language = languageString model.context.language }, navigateToPage machine model.context.language ] )

        ChangeLanguage language ->
            ( { model | context = changeLanguage (languageFromString language) model.context }, Cmd.none )


preferencesDecoder : Decode.Decoder { language : String, machine : String }
preferencesDecoder =
    Decode.map2 (\l m -> { language = l, machine = m }) (Decode.field "language" Decode.string) (Decode.field "machine" Decode.string)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    savedPreferences SavedPreferences



-- VIEW


view : Model -> Html Msg
view model =
    let
        primaryColor =
            Ui.rgba255 0x62 0x00 0xEE 1.0

        primaryColorLight =
            Ui.rgba255 0x62 0x00 0xEE 0.8

        primaryColorLighter =
            Ui.rgba255 0x62 0x00 0xEE 0.4

        selectionButton text page =
            UiIn.button
                [ Ui.padding 64
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
                ]
                { onPress = Just (PickMachine page), label = Ui.text text }

        --Widget.textButton (Material.textButton Style.palette) { text = text, onPress = Just (PickMachine page) }
    in
    Ui.layout [] <|
        Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 8 ]
            [ Ui.el [ Ui.alignRight, Ui.alignTop ] (languageSelect model.context ChangeLanguage)
            , Ui.row [ Ui.centerX, Ui.centerY, Ui.spacing 128, Ui.padding 16 ]
                [ selectionButton (translate Intl.Lavatrice model.context) "PageWashingMachine"
                , selectionButton (translate Intl.Essicatoio model.context) "PageDryingMachine"
                ]
            ]
