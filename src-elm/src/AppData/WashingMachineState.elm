module AppData.WashingMachineState exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Ports exposing (eventDecoder)


type State
    = Running
    | Stopped


type alias WashingMachineState =
    { state : State
    }


type ConnectionState
    = Disconnected
    | Connected WashingMachineState
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
    in
    Decode.oneOf
        [ Decode.andThen
            (\v ->
                case v of
                    "Disconnected" ->
                        Decode.succeed Disconnected

                    _ ->
                        Decode.fail <| "Invalid variant " ++ v
            )
            Decode.string
        , Decode.succeed Error
            |> Pipeline.required "Error" Decode.string
        , Decode.succeed Connected
            |> Pipeline.required "Connected" washingMachineStateDecoder
        ]
