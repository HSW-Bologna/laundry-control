module AppData.Parameter exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (IntlString)
import Context exposing (Context)
import Dict
import Element.Region exposing (description)
import Round


type ParameterUiType
    = Number
    | Option


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


validate : String -> Parameter a b -> Result IntlString ()
validate string par =
    case par.ui of
        Number ->
            String.toInt string
                |> Maybe.map
                    (\value ->
                        if value < par.min then
                            Err Intl.ValoreSottoIlMinimoConsentito

                        else if value > par.max then
                            Err Intl.ValoreSopraIlMassimoConsentito

                        else
                            Ok ()
                    )
                |> Maybe.withDefault (Err Intl.ValoreNonNumerico)

        Option ->
            Ok ()


options : b -> Context -> Parameter a b -> List String
options b context { min, max, format } =
    List.range min max
        |> List.map (format b context)


optionToIndex : b -> Context -> Parameter a b -> String -> Int
optionToIndex b context par option =
    options b context par
        |> List.indexedMap (\i s -> ( s, i ))
        |> Dict.fromList
        |> Dict.get option
        |> Maybe.withDefault par.default


fromString : b -> Context -> Parameter a b -> String -> Int
fromString b context par string =
    case par.ui of
        Number ->
            Maybe.withDefault par.default <| String.toInt string

        Option ->
            optionToIndex b context par string


formatPrice : Int -> Int -> String
formatPrice decimalDigits price =
    toFloat price
        / toFloat (10 ^ decimalDigits)
        |> Round.round decimalDigits
