module AppData.WashingMachineState exposing (..)

import Array exposing (Array)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Pipeline


type StateCode
    = Running
    | Stopped


type alias State =
    { name : String
    , state : StateCode
    }


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
    | Error String


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
                                1 ->
                                    Running

                                _ ->
                                    Stopped
                        )
                        Decode.int
                    )

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
        , Decode.succeed Error
            |> Pipeline.required "Error" Decode.string
        , Decode.succeed Connected
            |> Pipeline.requiredAt [ "Connected", "state" ] washingMachineStateDecoder
            |> Pipeline.requiredAt [ "Connected", "configuration" ] configurationDecoder
        ]
