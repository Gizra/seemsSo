module Item.View exposing (viewItemsTeaser)

import Backend.Entities exposing (ItemId)
import Backend.Restful exposing (EntityDictList)
import EveryDictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Backend.Item.Model exposing (Item)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


viewItemsTeaser : String -> EntityDictList ItemId Item -> Html msg
viewItemsTeaser baseUrl items =
    ul []
        (items
            |> EveryDictList.map
                (\itemId item ->
                    li []
                        -- @todo: Check what is itemId.
                        [ a [ href <| baseUrl ++ "item/" ++ toString itemId ] [ text item.name ]
                        ]
                )
            |> EveryDictList.values
        )
