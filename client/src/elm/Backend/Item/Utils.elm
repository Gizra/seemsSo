module Backend.Item.Utils
    exposing
        ( getComment
        , insertComments
        )

import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Model exposing (Item, ItemComment)
import Backend.Restful exposing (EntityDictList)
import Editable exposing (Editable)
import Editable.WebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)
import Maybe.Extra exposing (unwrap)
import StorageKey exposing (StorageKey)


getComment : ( StorageKey ItemId, StorageKey ItemCommentId ) -> EntityDictList ItemId Item -> Maybe (EditableWebData ItemComment)
getComment ( itemId, commentId ) items =
    case EveryDictList.get itemId items of
        Nothing ->
            Nothing

        Just item ->
            EveryDictList.get commentId item.comments


{-| Insert newly saved comments. If there is a comment with storage key `New`
we will empty the `comment` property.
That will be used in order to clear the comment form, and allow entering a new form.
-}
insertComments : ( StorageKey ItemId, StorageKey ItemCommentId ) -> EntityDictList ItemCommentId (EditableWebData ItemComment) -> EntityDictList ItemId Item -> EntityDictList ItemId Item
insertComments ( itemId, commentId ) itemComments items =
    case EveryDictList.get itemId items of
        Nothing ->
            items

        Just item ->
            let
                itemUpdated =
                    { item | comments = EveryDictList.append item.comments itemComments }

                itemWithFreshNew =
                    unwrap itemUpdated
                        (\editableWebData ->
                            let
                                value =
                                    editableWebData
                                        |> Editable.WebData.toEditable
                                        |> Editable.value

                                valueUpdated =
                                    { value | comment = "" }

                                itemCommentUpdated =
                                    editableWebData
                                        |> Editable.WebData.map (Editable.edit >> Editable.update valueUpdated)
                            in
                            { itemUpdated | comments = EveryDictList.insert commentId itemCommentUpdated itemUpdated.comments }
                        )
                        (getComment ( itemId, StorageKey.New ) items)
            in
            EveryDictList.insert itemId itemWithFreshNew items
