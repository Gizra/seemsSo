port module Backend.Item.Utils
    exposing
        ( getComment
        )

import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Model exposing (Item, ItemComment)
import Backend.Restful exposing (EntityDictList)
import Editable.WebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)
import StorageKey exposing (StorageKey)


getComment : ( StorageKey ItemId, StorageKey ItemCommentId ) -> EntityDictList ItemId Item -> Maybe (EditableWebData ItemComment)
getComment ( itemId, commentId ) items =
    case EveryDictList.get itemId items of
        Nothing ->
            Nothing

        Just item ->
            EveryDictList.get commentId item.comments
