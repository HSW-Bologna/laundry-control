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
            |> Pipeline.required "Connected" (Decode.succeed (WashingMachineState Stopped))
        ]
