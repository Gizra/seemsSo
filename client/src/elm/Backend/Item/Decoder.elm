module Backend.Item.Decoder
    exposing
        ( decodeItems
        )

import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Model exposing (Item, ItemComment)
import Backend.Restful exposing (EntityDictList, decodeId)
import EveryDictList exposing (decodeArray2, empty)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import User.Decoder exposing (decodeUserTuple)
import Utils.Json exposing (decodeEmptyArrayAs, decodeInt)


decodeItems : Decoder (EntityDictList ItemId Item)
decodeItems =
    oneOf
        [ decodeArray2 (decodeId ItemId) decodeItem
        , decodeEmptyArrayAs empty
        ]


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "name" string
        |> custom decodeItemComments


decodeItemComments : Decoder (EntityDictList ItemCommentId ItemComment)
decodeItemComments =
    oneOf
        [ decodeArray2 (decodeId ItemCommentId) decodeItemComment
        , decodeEmptyArrayAs EveryDictList.empty
        ]


decodeItemComment : Decoder ItemComment
decodeItemComment =
    (decode ItemComment
        |> custom decodeUserTuple
        |> required "comment" string
        |> required "created" decodeDate
    )
        |> andThen (\val -> succeed <| EditableWebData.create val)
