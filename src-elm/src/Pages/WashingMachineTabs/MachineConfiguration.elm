module Pages.WashingMachineTabs.MachineConfiguration exposing (..)

import AppData.WashingMachineConfiguration exposing (MachineConfiguration, MachineParameters)
import Context exposing (Context, translate)
import Element as Ui
import Element.Font as Font
import Element.Input as Input


type alias Model =
    { context : Context
    , config : MachineConfiguration
    , configName : String
    }


buildModel : Context -> MachineConfiguration -> Model
buildModel context config =
    Model context config config.parmac.name


type Msg
    = ConfigNameChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ConfigNameChange name ->
            -- TODO: limita la dimensione a 32 caratteri
            { model | configName = name, config = MachineConfiguration <| MachineParameters name }


view : Model -> Ui.Element Msg
view model =
    Ui.column [ Ui.width Ui.fill, Ui.height Ui.fill, Ui.padding 16 ]
        [ Ui.paragraph [ Font.size 32 ] [ Ui.text (translate "parametri_macchina" model.context) ]
        , Input.text [] { onChange = ConfigNameChange, text = model.configName, placeholder = Nothing, label = Input.labelHidden "name" }
        ]
