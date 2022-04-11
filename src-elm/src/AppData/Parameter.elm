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


type alias Parameter a b =
    { get : a -> Int
    , set : Int -> a -> a
    , min : Int
    , max : Int
    , default : Int
    , description : IntlString
    , format : b -> Context -> Int -> String
    , ui : ParameterUiType
    }


validate : Parameter a b -> Int -> Result IntlString Int
validate par value =
    if value < par.min then
        Err Intl.ValoreSottoIlMinimoConsentito

    else if value > par.max then
        Err Intl.ValoreSopraIlMassimoConsentito

    else
        Ok value


options : b -> Context -> Parameter a b -> List String
options b context { min, max, format } =
    List.range min max
        |> List.map (format b context)


indexToOption : b -> Context -> Parameter a b -> Int -> String
indexToOption b context par value =
    options b context par
        |> Array.fromList
        |> Array.get value
        |> Maybe.withDefault (translate Intl.Errore context)


optionToIndex : b -> Context -> Parameter a b -> String -> Maybe Int
optionToIndex b context par option =
    options b context par
        |> List.indexedMap (\i s -> ( s, i ))
        |> Dict.fromList
        |> Dict.get option


formatPrice : Int -> Int -> String
formatPrice decimalDigits price =
    toFloat price
        / toFloat (10 ^ decimalDigits)
        |> Round.round decimalDigits
