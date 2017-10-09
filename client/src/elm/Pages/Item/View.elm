module Pages.Item.View exposing (..)

import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Maybe.Extra exposing (isJust)
import User.Model exposing (CurrentUser(..))
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : String -> CurrentUser -> Html msg
view baseUrl currentUser =
    div []
        [ text "Item page"

        -- viewItemComments muser model.comments
        -- , Html.map MsgItemComment <| ItemComment.View.view baseUrl muser model.itemComment
        ]
