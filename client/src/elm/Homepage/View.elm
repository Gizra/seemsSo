module Contact.View exposing (..)

import App.Types exposing (Language(..))
import Contact.Model exposing (Contact, ContactId, DictListContact, Model, Msg(..))
import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (string)
import Translate exposing (TranslationId(..), translate)
import Utils.Html exposing (colorToString, divider, sectionDivider, showIf, showMaybe)


view : Model -> Html Msg
view model =
    div [] [ text "Homepage" ]
