module Pages.PageSelection exposing (..)

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
import Ports exposing (navigateToPage)
import Widget as Widget
import Widget.Material as Material



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PickMachine machine ->
            ( model, navigateToPage machine model.context.language )

        ChangeLanguage language ->
            ( { model | context = changeLanguage (languageFromString language) model.context }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



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

        --Widget.textButton (Material.textButton Material.defaultPalette) { text = text, onPress = Just (PickMachine page) }
    in
    Ui.layout [] <|
        Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 8 ]
            [ Ui.el [ Ui.alignRight, Ui.alignTop ] (languageSelect model.context ChangeLanguage)
            , Ui.row [ Ui.centerX, Ui.centerY, Ui.spacing 128, Ui.padding 16 ]
                [ selectionButton (translate Intl.Lavatrice model.context) "PageWashingMachine"
                , selectionButton (translate Intl.Essicatoio model.context) "PageDryingMachine"
                ]
            ]
