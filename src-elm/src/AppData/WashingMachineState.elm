module AppData.WashingMachineState exposing (..)

import Array exposing (Array)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Pipeline


type StateCode
    = Running
    | Stopped
    | Paused
    | ForcedDrain
    | Braking


type alias State =
    { name : String
    , state : StateCode
    , credit : Int
    , cycleNumber : Int
    , stepType : Int
    , stepNumber : Int
    , alarmCode : Int
    , portholeOpen : Bool
    , cycleRemaining : Int
    , stepRemaining : Int
    , stepCount : Int
    , sensors : Sensors
    }


type alias Sensors =
    { temperature : Int, level : Int, speed : Int }


type alias ProgramPreview =
    { name : String
    , washType : Int
    }


type alias Configuration =
    { name : String
    , version : String
    , machines : List String
    , programs : Array ProgramPreview
    }


type ConnectionState
    = Disconnected
    | Connected State Configuration
    | Error


connectionStateUpdateDecoder : Decode.Decoder ConnectionState
connectionStateUpdateDecoder =
    let
        washingMachineStateDecoder : Decode.Decoder State
        washingMachineStateDecoder =
            Decode.succeed State
                |> Pipeline.required "name" Decode.string
                |> Pipeline.required "state"
                    (Decode.map
                        (\i ->
                            case i of
                                0 ->
                                    Stopped

                                1 ->
                                    Running

                                2 ->
                                    Paused

                                3 ->
                                    ForcedDrain

                                6 ->
                                    Braking

                                _ ->
                                    Stopped
                        )
                        Decode.int
                    )
                |> Pipeline.required "credit" Decode.int
                |> Pipeline.required "cycle" Decode.int
                |> Pipeline.required "step_code" Decode.int
                |> Pipeline.required "step_number" Decode.int
                |> Pipeline.required "alarm_code" Decode.int
                |> Pipeline.required "porthole_open" Decode.bool
                |> Pipeline.required "cycle_remaining" Decode.int
                |> Pipeline.required "step_remaining" Decode.int
                |> Pipeline.required "step_count" Decode.int
                |> Pipeline.custom (Decode.map3 Sensors (Decode.field "temperature" Decode.int) (Decode.field "level" Decode.int) (Decode.field "speed" Decode.int))

        programPreviewDecoder : Decode.Decoder ProgramPreview
        programPreviewDecoder =
            Decode.succeed ProgramPreview
                |> Pipeline.required "name" Decode.string
                |> Pipeline.required "wash_type" Decode.int

        configurationDecoder : Decode.Decoder Configuration
        configurationDecoder =
            Decode.succeed Configuration
                |> Pipeline.required "name" Decode.string
                |> Pipeline.required "app_version" Decode.string
                |> Pipeline.required "machines" (Decode.list Decode.string)
                |> Pipeline.required "programs" (Decode.array programPreviewDecoder)
    in
    Decode.oneOf
        [ Decode.null Disconnected
        , Decode.succeed Connected
            |> Pipeline.requiredAt [ "Connected", "state" ] washingMachineStateDecoder
            |> Pipeline.requiredAt [ "Connected", "configuration" ] configurationDecoder
        , Decode.andThen
            (\s ->
                if s == "Error" then
                    Decode.succeed Error

                else
                    Decode.fail "Invalid error"
            )
            Decode.string
        ]
