module Context exposing (..)

import AUTOGEN_FILE_translations as Intl exposing (IntlString, Language, languageFromString)


type alias Context =
    { language : Language }


changeLanguage : Language -> Context -> Context
changeLanguage language _ =
    Context language


translate : IntlString -> Context -> String
translate key { language } =
    Intl.translate language key
