module Pages.Item.View exposing (..)

import DictList
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.View exposing (viewItemComments)
import Maybe.Extra exposing (isJust)
import Pages.Item.Model exposing (Model, Msg(..))
import User.Model exposing (User)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : String -> Maybe User -> Model -> Html Msg
view baseUrl muser model =
    div []
        [ viewItemComments muser model.comments
        , Html.map MsgItemComment <| ItemComment.View.view baseUrl muser model.itemComment
        ]
