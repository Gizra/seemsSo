module ItemComment.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.Model exposing (Model)
import User.Model exposing (User)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : Maybe User -> Model -> Html msg
view muser items =
    div []
        [ text "Item Comment" ]
