port module Ports exposing
    ( decodeEvent
    , getRemoteMachineConfiguration
    , sendRemoteMachineConfiguration
    , navigateHome
    , navigateToPage
    , preferences
    , searchMachines
    , washingMachineHttpConnect
    )

import AUTOGEN_FILE_translations exposing (Language, languageString)
import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Array exposing (Array)


languageKey : String
languageKey =
    "language"


pageKey : String
pageKey =
    "page"


machineKey : String
machineKey =
    "machine"



-- Communication with the Rust backend


port backendPort : Encode.Value -> Cmd msg



-- Communication with the JS backend


port navigateTo : { page : String, language : String } -> Cmd msg


navigateHome : Language -> Cmd msg
navigateHome language =
    navigateTo { page = "PageSelection", language = languageString language }


navigateToPage : String -> Language -> Cmd msg
navigateToPage page language =
    navigateTo { page = page, language = languageString language }


preferencesVariant : String
preferencesVariant =
    "Preferences"


preferences : { language : String, machine : String } -> Cmd msg
preferences { language, machine } =
    Encode.dict identity Encode.string (Dict.fromList [ ( languageKey, language ), ( machineKey, machine ) ])
        |> (\v -> Encode.object [ ( preferencesVariant, v ) ])
        |> backendPort


searchMachinesVariant : String
searchMachinesVariant =
    "SearchMachines"


searchMachines : Cmd msg
searchMachines =
    Encode.string searchMachinesVariant
        |> backendPort


washingMachineHttpConnectVariant : String
washingMachineHttpConnectVariant =
    "WashingMachineHttpConnect"


washingMachineHttpConnect : String -> Cmd msg
washingMachineHttpConnect machine =
    Encode.object [ ( washingMachineHttpConnectVariant, Encode.string machine ) ]
        |> backendPort


getRemoteMachineConfigurationVariant : String
getRemoteMachineConfigurationVariant =
    "GetRemoteMachineConfiguration"


getRemoteMachineConfiguration : String -> Cmd msg
getRemoteMachineConfiguration machine =
    Encode.object [ ( getRemoteMachineConfigurationVariant, Encode.string machine ) ]
        |> backendPort

sendRemoteMachineConfigurationVariant : String
sendRemoteMachineConfigurationVariant =
    "SendMachineConfiguration"


sendRemoteMachineConfiguration : String -> Array Int -> Cmd msg
sendRemoteMachineConfiguration name bytes =
    Encode.object [ ( sendRemoteMachineConfigurationVariant, Encode.object [ ( "name", Encode.string name ), ( "bytes", Encode.array Encode.int bytes ) ] ) ]
        |> backendPort



decodeEvent : Decode.Decoder a -> Encode.Value -> Result Decode.Error a
decodeEvent decoder =
    Decode.decodeValue decoder
