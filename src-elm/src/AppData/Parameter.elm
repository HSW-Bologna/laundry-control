module AppData.Parameter exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (IntlString)
import Array
import Context exposing (Context, translate)
import Dict
import Element.Region exposing (description)
import Round


type ParameterUiType
    = Number
    | Option
    | Price


type alias Parameter db info =
    { get : db -> Int
    , set : Int -> db -> db
    , min : Int
    , max : Int
    , default : Int
    , description : IntlString
    , format : info -> Context -> Int -> String
    , ui : ParameterUiType
    }


validate : Parameter db info -> Int -> Result IntlString Int
validate par value =
    if value < par.min then
        Err Intl.ValoreSottoIlMinimoConsentito

    else if value > par.max then
        Err Intl.ValoreSopraIlMassimoConsentito

    else
        Ok value


options : info -> Context -> Parameter db info -> List String
options info context { min, max, format } =
    List.range min max
        |> List.map (format info context)


indexToOption : info -> Context -> Parameter db info -> Int -> String
indexToOption info context par value =
    options info context par
        |> Array.fromList
        |> Array.get value
        |> Maybe.withDefault (translate Intl.Errore context)


optionToIndex : info -> Context -> Parameter db info -> String -> Maybe Int
optionToIndex info context par option =
    options info context par
        |> List.indexedMap (\i s -> ( s, i ))
        |> Dict.fromList
        |> Dict.get option


formatPrice : Int -> Int -> String
formatPrice decimalDigits price =
    toFloat price
        / toFloat (10 ^ decimalDigits)
        |> Round.round decimalDigits
