module Item.View exposing (viewItemsTeaser)

import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Item.Model exposing (EveryDictListItems, Item)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


viewItemsTeaser : String -> EveryDictListItems -> Html msg
viewItemsTeaser baseUrl items =
    div []
        (items
            |> DictList.map
                (\itemId item ->
                    a [ href <| baseUrl ++ "item/" ++ toString itemId ] [ text item.name ]
                )
            |> DictList.values
        )
