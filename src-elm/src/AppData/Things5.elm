module AppData.Things5 exposing (..)

import Json.Decode as Decode


type alias Device =
    { id : String
    , name : String
    }


deviceDecoder : Decode.Decoder Device
deviceDecoder =
    Decode.map2 Device (Decode.field "id" Decode.string) (Decode.field "name" Decode.string)
