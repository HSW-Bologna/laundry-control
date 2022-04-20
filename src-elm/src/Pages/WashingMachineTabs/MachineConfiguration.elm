module Pages.WashingMachineTabs.MachineConfiguration exposing (..)

import AUTOGEN_FILE_translations as Intl
import AppData.WashingMachineConfiguration exposing (MachineConfiguration, MachineParameter, changeName)
import AppWidgets.AppWidgets as AppWidgets
import AppWidgets.Style as Style
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import Element.Input as Input
import Widget as Widget
import Widget.Material as Material


type alias SharedModel a =
    { a | context : Context }


type alias Model =
    { config : MachineConfiguration
    , selected : Maybe ( MachineParameter, AppWidgets.ParameterModificationData )
    }


buildModel : MachineConfiguration -> Model
buildModel config =
    Model config Nothing


type Msg
    = ConfigNameChange String
    | SelectParameter MachineParameter
    | UnselectParameter
    | ParameterChange String
    | ParameterConfirm MachineParameter Int


update : Msg -> SharedModel a -> Model -> Model
update msg { context } ({ config } as model) =
    case msg of
        ConfigNameChange name ->
            -- TODO: limita la dimensione a 32 caratteri
            { model | config = changeName name config }

        SelectParameter par ->
            { model | selected = Just ( par, AppWidgets.parameterModificationData config.parmac config.parmac context par ) }

        UnselectParameter ->
            { model | selected = Nothing }

        ParameterChange text ->
            { model | selected = Maybe.map (\( par, data ) -> ( par, AppWidgets.validateParameterInput text data )) model.selected }

        ParameterConfirm par value ->
            { model | config = { config | parmac = par.set value config.parmac }, selected = Nothing }



--ParameterSelected par ->


view : SharedModel a -> Model -> Ui.Element Msg
view { context } model =
    let
        parmac =
            model.config.parmac

        modals =
            Maybe.map
                (\( p, t ) ->
                    [ { onDismiss = Just UnselectParameter
                      , content =
                            AppWidgets.parameterModificationDialog
                                { info = model.config.parmac
                                , context = context
                                , textChange = ParameterChange
                                , dismiss = UnselectParameter
                                , confirm = ParameterConfirm
                                , modData = t
                                , par = p
                                , priceMultiplier = model.config.parmac.priceDecimalDigits
                                }
                      }
                    ]
                )
                model.selected
                |> Maybe.withDefault []
                |> Widget.singleModal
    in
    AppWidgets.scrollbarYEl (modals ++ [ Ui.width Ui.fill, Ui.height Ui.fill ]) <|
        Ui.column
            [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16, Ui.spacing 8 ]
            [ Ui.paragraph [ Font.size 32 ] [ Ui.text (translate Intl.ParametriMacchina context) ]
            , Input.text [] { onChange = ConfigNameChange, text = model.config.parmac.name, placeholder = Nothing, label = Input.labelHidden "name" }
            , model.config.parmacMetadata
                |> List.indexedMap
                    (AppWidgets.parameter parmac parmac context SelectParameter)
                |> List.map Widget.asItem
                |> Widget.itemList (Material.cardColumn Style.palette)
            ]
