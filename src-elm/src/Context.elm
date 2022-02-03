module Context exposing (..)

import Intl
import Json.Decode as Decode
import Maybe.Extra as Maybe


type alias Context =
    { language : Intl.Language, intl : Maybe Intl.Intl }


changeLanguage : Intl.Language -> Context -> Context
changeLanguage language { intl } =
    Context language intl


getTransl : String -> { a | context : Context } -> String
getTransl key { context } =
    Maybe.map (Intl.translate context.language key) context.intl
        |> Maybe.withDefault "INVALID INTL JSON"


translate : String -> Context -> String
translate key { intl, language } =
    Maybe.map (Intl.translate language key) intl
        |> Maybe.withDefault "INVALID INTL JSON"


fromJsonValue : String -> Decode.Value -> Context
fromJsonValue language value =
    Decode.decodeValue Intl.intlDecoder value
        |> Result.toMaybe
        |> Maybe.join
        |> Context (Intl.languageFromString language)
