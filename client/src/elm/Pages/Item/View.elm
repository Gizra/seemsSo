module Pages.Item.View exposing (..)

import App.Types exposing (BackendUrl)
import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Restful exposing (EntityDictList)
import DictList
import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.View exposing (viewItemComments)
import Maybe.Extra exposing (isJust, unwrap)
import StorageKey exposing (StorageKey(Existing))
import User.Model exposing (CurrentUser(..))
import Utils.Html exposing (divider, emptyNode, sectionDivider, showIf, showMaybe)


view : BackendUrl -> CurrentUser -> EntityDictList ItemId Item -> ItemId -> Html msg
view backendUrl currentUser items currentItemId =
    unwrap emptyNode
        (\item ->
            div []
                [ h1 [] [ text item.name ]
                , pre [] [ text <| toString items ]
                , viewItemComments currentUser item.comments

                -- , Html.map MsgItemComment <| ItemComment.View.view baseUrl muser model.itemComment
                ]
        )
        (EveryDictList.get (Existing currentItemId) items)
