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


type alias Statistics =
    { cycles : Int
    , interruptedCycles : Int
    , loopCycles : Int
    , onTime : Int
    , workTime : Int
    , rotationTime : Int
    , heatingTime : Int
    , coldWaterTime : Int
    , warmWaterTime : Int
    , recoveryWaterTime : Int
    , fluxWaterTime : Int
    , portholeClosings : Int
    , portholeOpenings : Int
    , soapTimes : List Int
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
    | Connected State Configuration Statistics
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

        statisticsDecoder : Decode.Decoder Statistics
        statisticsDecoder =
            Decode.succeed Statistics
                |> Pipeline.required "cycles" Decode.int
                |> Pipeline.required "interrupted_cycles" Decode.int
                |> Pipeline.required "loop_cycles" Decode.int
                |> Pipeline.required "on_time" Decode.int
                |> Pipeline.required "work_time" Decode.int
                |> Pipeline.required "rotation_time" Decode.int
                |> Pipeline.required "heating_time" Decode.int
                |> Pipeline.required "cold_water_time" Decode.int
                |> Pipeline.required "warm_water_time" Decode.int
                |> Pipeline.required "recovery_water_time" Decode.int
                |> Pipeline.required "flux_water_time" Decode.int
                |> Pipeline.required "porthole_closings" Decode.int
                |> Pipeline.required "porthole_openings" Decode.int
                |> Pipeline.required "soap_times" (Decode.list Decode.int)
    in
    Decode.oneOf
        [ Decode.null Disconnected
        , Decode.succeed Connected
            |> Pipeline.requiredAt [ "Connected", "state" ] washingMachineStateDecoder
            |> Pipeline.requiredAt [ "Connected", "configuration" ] configurationDecoder
            |> Pipeline.requiredAt [ "Connected", "stats" ] statisticsDecoder
        , Decode.andThen
            (\s ->
                if s == "Error" then
                    Decode.succeed Error

                else
                    Decode.fail "Invalid error"
            )
            Decode.string
        ]
