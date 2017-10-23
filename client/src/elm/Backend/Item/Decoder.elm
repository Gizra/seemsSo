module Backend.Item.Decoder
    exposing
        ( decodeItemComments
        , decodeItems
        , deocdeItemIdAndComments
        )

import Amount exposing (decodeAmount)
import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Model exposing (Company, Item, ItemComment)
import Backend.Restful exposing (EntityDictList, EntityId, decodeId, toEntityId)
import Date
import Editable.WebData as EditableWebData exposing (EditableWebData)
import EveryDictList exposing (decodeArray2, empty)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import StorageKey exposing (StorageKey(Existing))
import User.Decoder exposing (decodeUserTuple)
import User.Model exposing (CurrentUser(..))
import Utils.Json exposing (decodeDate, decodeEmptyArrayAs, decodeInt)


decodeItems : CurrentUser -> Decoder (EntityDictList ItemId Item)
decodeItems currentUser =
    oneOf
        [ decodeArray2 (field "item" decodeStorageKeyAsEntityId) (decodeItem currentUser)
        , field "item" <| decodeEmptyArrayAs empty
        ]


decodeItem : CurrentUser -> Decoder Item
decodeItem currentUser =
    decode Item
        |> requiredAt [ "item", "name" ] string
        |> optionalAt [ "item", "comments" ] (decodeItemComments currentUser) EveryDictList.empty
        |> requiredAt [ "item", "price" ] decodeAmount
        |> required "company" decodeCompany


decodeCompany : Decoder Company
decodeCompany =
    decode Company
        |> required "name" string


decodeItemComments : CurrentUser -> Decoder (EntityDictList ItemCommentId (EditableWebData ItemComment))
decodeItemComments currentUser =
    oneOf
        [ decodeArray2 decodeStorageKeyAsEntityId decodeItemComment
        , decodeEmptyArrayAs EveryDictList.empty
        ]
        |> andThen
            (\dictList ->
                -- Add `New` only of Authnticated users.
                (case currentUser of
                    Authenticated userTuple ->
                        let
                            emptyCommentItem =
                                EditableWebData.create
                                    { user = userTuple
                                    , comment = ""

                                    -- @todo: We should consider making another type for CommentNotCreated
                                    -- that won't hold the `user` and `created` as they are not relevant
                                    -- while the comment isn't saved yet.
                                    , created = Date.fromTime 0
                                    }
                        in
                        dictList
                            |> EveryDictList.insert StorageKey.New emptyCommentItem

                    Anonymous ->
                        dictList
                )
                    |> succeed
            )


decodeStorageKeyAsEntityId : Decoder (StorageKey (EntityId a))
decodeStorageKeyAsEntityId =
    decodeId toEntityId |> andThen (\val -> succeed <| Existing val)


decodeItemComment : Decoder (EditableWebData ItemComment)
decodeItemComment =
    (decode ItemComment
        |> custom decodeUserTuple
        |> required "comment" string
        |> required "created" decodeDate
    )
        |> andThen (\val -> succeed <| EditableWebData.create val)


deocdeItemIdAndComments : CurrentUser -> Decoder ( StorageKey ItemId, EntityDictList ItemCommentId (EditableWebData ItemComment) )
deocdeItemIdAndComments currentUser =
    decode (,)
        |> required "itemId" (decodeInt |> andThen (\val -> toEntityId val |> Existing |> succeed))
        |> required "comments" (decodeItemComments currentUser)
