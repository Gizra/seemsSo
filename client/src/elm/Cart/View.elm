module Cart.View exposing (..)

import Cart.Model exposing (Model, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Item.View exposing (viewItemsTeaser)
import Json.Encode exposing (string)
import User.Model exposing (User)
import User.View
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : Model -> Html Msg
view model =
    if List.isEmpty model.items then
        viewEmptyCart
    else
        viewCart model


viewEmptyCart : Html Msg
viewEmptyCart =
    div [] [ text "Empty cart" ]


viewCart : Model -> Html Msg
viewCart model =
    div []
        [ ul [] (List.map (\itemId -> li [] [ text <| toString itemId ]) model.items)
        ]
