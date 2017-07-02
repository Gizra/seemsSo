module ItemComment.View exposing (view)

import ItemComment.Model exposing (ItemComment)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


type alias Model =
    ItemComment


view : Model -> Html msg
view items =
    div []
        [ text "Item Comment" ]
