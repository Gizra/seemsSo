module Pages.Item.View exposing (viewItemsTeaser)

import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Pages.Item.Model exposing (Model)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : String -> Model -> Html msg
view baseUrl items =
    div [] []
