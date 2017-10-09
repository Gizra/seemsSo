module Backend.Item.Decoder
    exposing
        ( decodeItemComments
        , decodeItems
        )

import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Model exposing (Item, ItemComment)
import Backend.Restful exposing (EntityDictList, EntityId, decodeId, toEntityId)
import Editable.WebData as EditableWebData exposing (EditableWebData)
import EveryDictList exposing (decodeArray2, empty)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import StorageKey exposing (StorageKey(Existing))
import User.Decoder exposing (decodeUserTuple)
import Utils.Json exposing (decodeDate, decodeEmptyArrayAs, decodeInt)


decodeItems : Decoder (EntityDictList ItemId Item)
decodeItems =
    oneOf
        [ decodeArray2 decodeStorageKeyAsEntityId decodeItem
        , decodeEmptyArrayAs empty
        ]


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "name" string
        |> custom decodeItemComments


decodeItemComments : Decoder (EntityDictList ItemCommentId (EditableWebData ItemComment))
decodeItemComments =
    oneOf
        [ decodeArray2 decodeStorageKeyAsEntityId decodeItemComment
        , decodeEmptyArrayAs EveryDictList.empty
        ]


decodeItemComment : Decoder (EditableWebData ItemComment)
decodeItemComment =
    (decode ItemComment
        |> custom decodeUserTuple
        |> required "comment" string
        |> required "created" decodeDate
    )
        |> andThen (\val -> succeed <| EditableWebData.create val)


decodeStorageKeyAsEntityId : Decoder (StorageKey (EntityId a))
decodeStorageKeyAsEntityId =
    decodeId toEntityId |> andThen (\val -> succeed <| Existing val)
