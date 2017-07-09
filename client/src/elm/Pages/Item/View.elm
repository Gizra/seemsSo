module Pages.Item.View exposing (..)

import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Pages.Item.Model exposing (Model)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)
import User.Model exposing (User)


view : String -> Maybe User -> Model -> Html msg
view baseUrl muser model =
    div [] [ text "PAges.Item.View" ]
