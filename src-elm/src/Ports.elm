port module Ports exposing (eventDecoder, navigateHome, navigateToPage)

import AUTOGEN_FILE_translations exposing (Language, languageString)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline


port navigateTo : { page : String, language : String } -> Cmd msg


navigateHome : Language -> Cmd msg
navigateHome language =
    navigateTo { page = "PageSelection", language = languageString language }


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
