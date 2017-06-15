module Homepage.View exposing (..)

import Homepage.Model exposing (Model, Msg(..))
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Item.View exposing (viewItemsTeaser)
import Json.Encode exposing (string)
import User.Model exposing (User)
import User.View
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : Maybe User -> Model -> Html Msg
view muser model =
    div []
        [ User.View.view muser
        , viewItemsTeaser model.items
        ]
