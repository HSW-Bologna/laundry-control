module Pages.PageDryingMachine exposing (..)

import Browser
import Context exposing (Context, getTransl)
import Element as Ui
import Html exposing (Html)
import Json.Decode as Decode
import Ports exposing (navigateHome)
import Widget as Widget
import Widget.Icon as Icon
import Widget.Material as Material
import AppWidgets.AppWidgets as AppWidgets



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


init : ( Decode.Value, String ) -> ( Model, Cmd Msg )
init ( value, language ) =
    ( { context = Context.fromJsonValue language value
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
            [ Widget.menuBar (Material.menuBar Material.defaultPalette)
                { title =
                    getTransl "essicatoio" model
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
