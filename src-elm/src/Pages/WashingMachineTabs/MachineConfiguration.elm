module Pages.WashingMachineTabs.MachineConfiguration exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.Parameter as Parameter exposing (Parameter)
import AppData.WashingMachineConfiguration exposing (MachineConfiguration, MachineParameter, MachineParameters, changeName)
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import Element.Input as Input
import FontAwesome.Solid exposing (scroll)
import Widget as Widget
import Widget.Material as Material


type alias Model =
    { context : Context
    , config : MachineConfiguration
    , selected : Maybe ( MachineParameter, String )
    }


buildModel : Context -> MachineConfiguration -> Model
buildModel context config =
    Model context config Nothing


type Msg
    = ConfigNameChange String
    | SelectParameter MachineParameter
    | UnselectParameter
    | ParameterChange MachineParameter String
    | ParameterConfirm MachineParameter Int


update : Msg -> Model -> Model
update msg ({ config } as model) =
    case msg of
        ConfigNameChange name ->
            -- TODO: limita la dimensione a 32 caratteri
            { model | config = changeName name config }

        SelectParameter par ->
            { model | selected = Just ( par, par.get config.parmac |> String.fromInt ) }

        UnselectParameter ->
            { model | selected = Nothing }

        ParameterChange par text ->
            case par.ui of
                Parameter.Number ->
                    case validateString text of
                        Just validated ->
                            { model | selected = Just ( par, validated ) }

                        Nothing ->
                            model

                Parameter.Option ->
                    { model | selected = Just ( par, text ) }

        ParameterConfirm par value ->
            { model | config = { config | parmac = par.set value config.parmac }, selected = Nothing }


validateString : String -> Maybe String
validateString string =
    case string of
        "" ->
            Just ""

        value ->
            String.toInt value
                |> Maybe.map (\_ -> value)



--ParameterSelected par ->


view : Model -> Ui.Element Msg
view model =
    let
        modals =
            Maybe.map
                (\( p, t ) ->
                    [ { onDismiss = Just UnselectParameter
                      , content =
                            AppWidgets.parameterModificationDialog model.config.parmac
                                model.context
                                ParameterChange
                                UnselectParameter
                                ParameterConfirm
                                t
                                p
                      }
                    ]
                )
                model.selected
                |> Maybe.withDefault []
                |> Widget.singleModal
    in
    AppWidgets.scrollbarYEl (modals ++ [ Ui.width Ui.fill, Ui.height Ui.fill ]) <|
        Ui.column
            [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.scrollbarY, Ui.spacing 8 ]
            [ Ui.paragraph [ Font.size 32 ] [ Ui.text (translate Intl.ParametriMacchina model.context) ]
            , Input.text [] { onChange = ConfigNameChange, text = model.config.parmac.name, placeholder = Nothing, label = Input.labelHidden "name" }
            , model.config.parmacMetadata
                |> List.map
                    (AppWidgets.parameter model.context model.config.parmac model.config.parmac SelectParameter)
                |> List.map Widget.asItem
                |> Widget.itemList (Material.cardColumn Style.palette)
            ]
