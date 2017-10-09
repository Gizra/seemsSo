module ItemComment.Decoder
    exposing
        ( decodeEveryDictListItemComments
        , decodeItemComment
        , decodeItemCommentId
        )

import Backend.Entities exposing (ItemCommentId)
import Backend.Restful exposing (EntityDictList)
import EveryDictList exposing (decodeArray2, empty)
import Backend.ItemComment.Model exposing (ItemComment, ItemCommentId)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import User.Decoder exposing (decodeUser, decoderUserId)
import Utils.Json exposing (decodeDate, decodeEmptyArrayAs, decodeInt)


decodeEveryDictListItemComments : Decoder (EntityDictList ItemCommentId ItemComment)
decodeEveryDictListItemComments =
    oneOf
        [ decodeArray2 (decodeId ItemCommentId) decodeItemComment
        , decodeEmptyArrayAs EveryDictList.empty
        ]


decodeItemComment : Decoder ItemComment
decodeItemComment =
    decode ItemComment
        |> required "userId" decoderUserId
        |> custom decodeUser
        |> required "comment" string
        |> required "created" decodeDate
