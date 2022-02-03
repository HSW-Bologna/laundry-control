port module Ports exposing (navigateHome, navigateToPage)

import Intl exposing (Language, languageString)


port navigateTo : { page : String, language : String } -> Cmd msg


navigateHome : Language -> Cmd msg
navigateHome language =
    navigateTo { page = "PageSelection", language = languageString language }


navigateToPage : String -> Language -> Cmd msg
navigateToPage page language =
    navigateTo { page = page, language = languageString language }
