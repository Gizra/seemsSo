module Pages.Item.View exposing (..)

import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.View exposing (viewItemComments)
import Pages.Item.Model exposing (Model)
import User.Model exposing (User)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : String -> Maybe User -> Model -> Html msg
view baseUrl muser model =
    div []
        [ text "Pages.Item.View"
        , viewItemComments muser model.comments
        ]
