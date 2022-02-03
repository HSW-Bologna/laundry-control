module Intl exposing (Intl, Language(..), intlDecoder, languageString, translate, languageFromString)

import Array
import Dict exposing (Dict)
import Json.Decode as Decode


type Language
    = Italian
    | English


type alias Translation =
    { italian : String, english : String }


type alias Intl =
    Dict String Translation


languageString : Language -> String
languageString language =
    case language of
        Italian ->
            "Italiano"

        English ->
            "English"


languageFromString : String -> Language
languageFromString language =
    case language of
        "Italiano" ->
            Italian

        "English" ->
            English

        _ ->
            Italian


getTranslation : Language -> Translation -> String
getTranslation language { italian, english } =
    case language of
        Italian ->
            italian

        English ->
            english


translate : Language -> String -> Intl -> String
translate language k intl =
    Dict.get k intl
        |> Maybe.map (getTranslation language)
        |> Maybe.withDefault "MISSING TRANSLATION"


listToTranslation : List String -> Maybe Translation
listToTranslation list =
    let
        get i =
            Array.fromList list
                |> Array.get i
    in
    Maybe.map2 Translation (get 0) (get 1)


intlDecoder : Decode.Decoder (Maybe Intl)
intlDecoder =
    Decode.map listToTranslation (Decode.list Decode.string)
        |> Decode.dict
        |> Decode.map verifyIntl


verifyIntl : Dict String (Maybe Translation) -> Maybe Intl
verifyIntl =
    let
        insertMaybe : String -> Maybe Translation -> Maybe Intl -> Maybe Intl
        insertMaybe k v res =
            Maybe.map2 (\intl trans -> Dict.insert k trans intl) res v
    in
    Just Dict.empty
        |> Dict.foldl insertMaybe
