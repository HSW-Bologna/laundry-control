module AppData.WashingMachineState exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Pipeline
import Ports


type State
    = Running
    | Stopped


type alias WashingMachineState =
    { state : State
    }


type alias Metadata =
    { version : String
    , machines : List String
    }


type ConnectionState
    = Disconnected
    | Connected WashingMachineState Metadata
    | Error String


connectionStateUpdateDecoder : Decode.Decoder ConnectionState
connectionStateUpdateDecoder =
    let
        washingMachineStateDecoder : Decode.Decoder WashingMachineState
        washingMachineStateDecoder =
            Decode.succeed WashingMachineState
                |> Pipeline.required "state"
                    (Decode.map
                        (\i ->
                            case i of
                                1 ->
                                    Running

                                _ ->
                                    Stopped
                        )
                        Decode.int
                    )

        metadataDecoder : Decode.Decoder Metadata
        metadataDecoder =
            Decode.succeed Metadata
                |> Pipeline.required "app_version" Decode.string
                |> Pipeline.required "machines" (Decode.list Decode.string)
    in
    Decode.oneOf
        [ Decode.null Disconnected
        , Decode.succeed Error
            |> Pipeline.required "Error" Decode.string
        , Decode.succeed Connected
            |> Pipeline.requiredAt [ "Connected", "state" ] washingMachineStateDecoder
            |> Pipeline.requiredAt [ "Connected", "metadata" ] metadataDecoder
        ]
