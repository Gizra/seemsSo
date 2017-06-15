module Item.Decoder
    exposing
        ( decodeItems
        )

import Item.Model exposing (Item, ItemId, EveryDictListItems)
import DictList exposing (EveryDictList, decodeArray2, empty)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import Utils.Json exposing (decodeEmptyArrayAs)


decodeItems : Decoder EveryDictListItems
decodeItems =
    oneOf
        [ decodeArray2 (field "id" decodeItemId) decodeItem
        , decodeEmptyArrayAs EveryDictList.empty
        ]


decodeItemId : Decoder ItemId
decodeItemId =
    decodeInt |> Decode.map ItemId


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "name" string
