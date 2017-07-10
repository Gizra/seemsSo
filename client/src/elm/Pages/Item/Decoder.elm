module Pages.Item.Decoder
    exposing
        ( deocdeItemIdAndComments
        )

import Item.Model exposing (EveryDictListItems, Item, ItemId)
import ItemComment.Decoder exposing (decodeEveryDictListItemComments, decodeItemCommentId)
import ItemComment.Model exposing (EveryDictListItemComments, ItemComment, ItemCommentId)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import Utils.Json exposing (decodeEmptyArrayAs, decodeInt)


deocdeItemIdAndComments : Decoder ( ItemCommentId, EveryDictListItemComments )
deocdeItemIdAndComments =
    decode (,)
        |> custom decodeItemCommentId
        |> custom decodeEveryDictListItemComments
