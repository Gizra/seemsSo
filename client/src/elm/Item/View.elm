module Item.View exposing (viewItemsTeaser)

import App.Types exposing (Item)
import Backend.Entities exposing (ItemId)
import Backend.Restful exposing (EntityDictList)
import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Item.Model exposing (EveryDictListItems, Item)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


viewItemsTeaser : String -> EntityDictList ItemId Item -> Html msg
viewItemsTeaser baseUrl items =
    ul []
        (items
            |> DictList.map
                (\itemId item ->
                    li []
                        [ a [ href <| baseUrl ++ "item/" ++ toString itemId ] [ text item.name ]
                        ]
                )
            |> DictList.values
        )
