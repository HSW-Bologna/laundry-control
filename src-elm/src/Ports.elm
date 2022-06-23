port module Ports exposing
    ( clearAlarms
    , decodeEvent
    , getCurrentMachineConfiguration
    , navigateHome
    , navigateToPage
    , pauseProgram
    , restartProgram
    , searchMachines
    , selectRemoteMachineConfiguration
    , sendCurrentMachineConfiguration
    , startProgram
    , stopProgram
    , things5Login
    , washingMachineHttpConnect
    , washingMachineThings5Connect
    )

import AUTOGEN_FILE_translations exposing (Language, languageString)
import Array exposing (Array)
import Json.Decode as Decode
import Json.Encode as Encode



-- COMMUNICATION WITH THE JS BACKEND


port navigateTo : { page : String, language : String } -> Cmd msg


navigateHome : Language -> Cmd msg
navigateHome language =
    navigateTo { page = "PageSelection", language = languageString language }


navigateToPage : String -> Language -> Cmd msg
navigateToPage page language =
    navigateTo { page = page, language = languageString language }



-- COMMUNICATION WITH THE RUST BACKEND


port backendPort : Encode.Value -> Cmd msg


searchMachines : Cmd msg
searchMachines =
    let
        variant : String
        variant =
            "SearchMachines"
    in
    Encode.string variant
        |> backendPort


washingMachineHttpConnect : String -> Cmd msg
washingMachineHttpConnect machine =
    let
        variant : String
        variant =
            "WashingMachineHttpConnect"
    in
    Encode.object [ ( variant, Encode.string machine ) ]
        |> backendPort


washingMachineThings5Connect : String -> String -> Cmd msg
washingMachineThings5Connect token device_id =
    let
        variant : String
        variant =
            "WashingMachineThings5Connect"
    in
    Encode.object [ ( variant, Encode.object [ ( "token", Encode.string token ), ( "device_id", Encode.string device_id ) ] ) ]
        |> backendPort


things5Login : String -> String -> Cmd msg
things5Login username password =
    let
        variant : String
        variant =
            "Things5Login"
    in
    Encode.object [ ( variant, Encode.object [ ( "username", Encode.string username ), ( "password", Encode.string password ) ] ) ]
        |> backendPort


getCurrentMachineConfiguration : Cmd msg
getCurrentMachineConfiguration =
    let
        variant : String
        variant =
            "GetCurrentMachineConfiguration"
    in
    Encode.string variant
        |> backendPort


sendCurrentMachineConfiguration : Array Int -> Cmd msg
sendCurrentMachineConfiguration bytes =
    let
        variant : String
        variant =
            "SendCurrentMachineConfiguration"
    in
    Encode.object [ ( variant, Encode.array Encode.int bytes ) ]
        |> backendPort


selectRemoteMachineConfiguration : String -> Cmd msg
selectRemoteMachineConfiguration machine =
    let
        variant : String
        variant =
            "SelectMachineConfiguration"
    in
    Encode.object [ ( variant, Encode.string machine ) ]
        |> backendPort


startProgram : Int -> Cmd msg
startProgram index =
    let
        variant : String
        variant =
            "StartProgram"
    in
    Encode.object [ ( variant, Encode.int index ) ]
        |> backendPort


restartProgram : Cmd msg
restartProgram =
    let
        variant : String
        variant =
            "Restart"
    in
    Encode.string variant
        |> backendPort


pauseProgram : Cmd msg
pauseProgram =
    let
        variant : String
        variant =
            "Pause"
    in
    Encode.string variant
        |> backendPort


stopProgram : Cmd msg
stopProgram =
    let
        variant : String
        variant =
            "Stop"
    in
    Encode.string variant
        |> backendPort


clearAlarms : Cmd msg
clearAlarms =
    let
        variant : String
        variant =
            "ClearAlarms"
    in
    Encode.string variant
        |> backendPort


decodeEvent : Decode.Decoder a -> Encode.Value -> Result Decode.Error a
decodeEvent decoder =
    Decode.decodeValue decoder
