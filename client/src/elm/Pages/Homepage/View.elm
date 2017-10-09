module Pages.Homepage.View exposing (..)

import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Restful exposing (EntityDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import User.Model exposing (CurrentUser)


view : String -> CurrentUser -> EntityDictList ItemId Item -> Html msg
view baseUrl currentUser items =
    div []
        [ text "Homepage"

        -- viewItemsTeaser baseUrl model.items
        ]
