port module Ports exposing (eventDecoder, navigateHome, navigateToPage, preferences, decodeEvent)

import AUTOGEN_FILE_translations exposing (Language, languageString)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


port navigateTo : { page : String, language : String } -> Cmd msg


navigateHome : Language -> Cmd msg
navigateHome language =
    navigateTo { page = "PageSelection", language = languageString language }


port preferences : { language : String, machine : String } -> Cmd msg


navigateToPage : String -> Language -> Cmd msg
navigateToPage page language =
    navigateTo { page = page, language = languageString language }


eventDecoder : String -> Decode.Decoder Decode.Value
eventDecoder event =
    Decode.field "event" Decode.string
        |> Decode.andThen
            (\e ->
                if e == event then
                    Decode.field "payload" Decode.value

                else
                    Decode.fail "Incorrect event"
            )


decodeEvent : String -> Decode.Decoder a -> Encode.Value -> Result Decode.Error a
decodeEvent event decoder value =
    Decode.decodeValue (eventDecoder event) value |> Result.andThen (Decode.decodeValue decoder)
