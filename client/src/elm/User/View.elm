module User.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import User.Model exposing (User)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : Maybe User -> Html msg
view muser =
    case muser of
        Just user ->
            div [] [ text user.name ]

        Nothing ->
            div [] [ text "Anonymous" ]
