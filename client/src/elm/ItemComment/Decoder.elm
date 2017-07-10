module ItemComment.Decoder
    exposing
        ( decodeEveryDictListItemComments
        )

import EveryDictList exposing (decodeArray2, empty)
import ItemComment.Model exposing (EveryDictListItemComments, ItemComment, ItemCommentId)
import ItemComment.Model exposing (ItemCommentId)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import Utils.Json exposing (decodeEmptyArrayAs, decodeInt)


decodeEveryDictListItemComments : Decoder EveryDictListItemComments
decodeEveryDictListItemComments =
    oneOf
        [ decodeArray2 (field "id" decodeItemCommentId) decodeItemComment
        , decodeEmptyArrayAs EveryDictList.empty
        ]


decodeItemComment : Decoder ItemComment
decodeItemComment =
    decode ItemComment
        |> required "userId" decodeUs



-- { userId : UserId
-- , userName : String
-- , comment : String
-- , created : Date
-- }


decodeItemCommentId : Decoder ItemCommentId
decodeItemCommentId =
    decodeInt
        |> map ItemCommentId
