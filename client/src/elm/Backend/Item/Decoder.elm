module Backend.Item.Decoder
    exposing
        ( decodeItems
        )

import Backend.Entities exposing (ItemId)
import Backend.Restful exposing (EntityDictList, decodeId)
import EveryDictList exposing (decodeArray2, empty)
import Backend.Item.Model exposing (Item)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
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
