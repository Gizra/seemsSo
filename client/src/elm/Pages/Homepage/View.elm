module Pages.Homepage.View exposing (..)

import Backend.Restful exposing (EntityDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Item.View exposing (viewItemsTeaser)
import Json.Encode exposing (string)
import Pages.Homepage.Model exposing (Model, Msg(..))
import User.Model exposing (CurrentUser, User)
import User.View
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : String -> CurrentUser -> EntityDictList ItemId Item -> Html Msg
view baseUrl muser items =
    div []
        [ text "Homepage"

        -- viewItemsTeaser baseUrl model.items
        ]
