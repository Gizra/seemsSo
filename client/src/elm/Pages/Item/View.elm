module Pages.Item.View exposing (..)

import App.Types exposing (BackendUrl)
import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Restful exposing (EntityDictList)
import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.View exposing (viewItemComments)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Item.Model exposing (Model, Msg)
import StorageKey exposing (StorageKey(Existing))
import User.Model exposing (CurrentUser(..))
import Utils.Html exposing (divider, emptyNode, sectionDivider, showIf, showMaybe)


view : BackendUrl -> CurrentUser -> EntityDictList ItemId Item -> StorageKey ItemId -> Model -> Html Msg
view backendUrl currentUser items itemStorageKey model =
    unwrap emptyNode
        (\item ->
            div []
                [ h1 [] [ text item.name ]
                , Html.map Pages.Item.Model.MsgItemComment <| ItemComment.View.view backendUrl currentUser ( itemStorageKey, item ) StorageKey.New model.itemComment
                , viewItemComments currentUser item.comments
                ]
        )
        (EveryDictList.get itemStorageKey items)
