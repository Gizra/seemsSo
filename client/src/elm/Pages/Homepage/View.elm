module Pages.Homepage.View exposing (..)

import App.Types exposing (BackendUrl(..))
import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Restful exposing (EntityDictList)
import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import User.Model exposing (CurrentUser)


view : BackendUrl -> CurrentUser -> EntityDictList ItemId Item -> Html msg
view backendUrl currentUser items =
    div []
        [ viewItemsTeaser backendUrl items
        ]


viewItemsTeaser : BackendUrl -> EntityDictList ItemId Item -> Html msg
viewItemsTeaser (BackendUrl backendUrl) items =
    ul []
        (items
            |> EveryDictList.map
                (\itemId item ->
                    li []
                        [ a [ href <| backendUrl ++ "item/" ++ toString itemId ] [ text item.name ]
                        ]
                )
            |> EveryDictList.values
        )
