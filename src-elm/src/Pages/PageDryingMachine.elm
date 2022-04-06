module Pages.PageDryingMachine exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (languageFromString)
import AppWidgets.Style as Style
import Browser
import Context exposing (Context, translate)
import Element as Ui
import Html exposing (Html)
import Ports exposing (navigateHome)
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
    { context : Context
    , leftDrawerVisible : Bool
    }


init : String -> ( Model, Cmd Msg )
init language =
    ( { context = Context <| languageFromString language
      , leftDrawerVisible = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Back
    | SelectMenubar Int
    | LoadMachineFromLocal
    | LeftDrawerToggle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Back ->
            ( model, navigateHome model.context.language )

        SelectMenubar n ->
            ( model, Cmd.none )

        LoadMachineFromLocal ->
            ( model, Cmd.none )

        LeftDrawerToggle ->
            ( { model | leftDrawerVisible = not model.leftDrawerVisible }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Ui.layout [] <|
        Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill ]
            [ Widget.menuBar (Material.menuBar Style.palette)
                { title =
                    translate Intl.Essicatoio model.context
                        |> Ui.text
                        |> Ui.el []
                , deviceClass = Ui.Desktop
                , openLeftSheet = Just LeftDrawerToggle
                , openRightSheet = Nothing
                , openTopSheet = Nothing
                , primaryActions =
                    []
                , search = Nothing
                }
            , Ui.row [ Ui.width Ui.fill, Ui.height Ui.fill ]
                [ if model.leftDrawerVisible then
                    --AppWidgets.leftDrawer model.context Back Back Back Back
                    Ui.none

                  else
                    Ui.none
                ]
            ]
