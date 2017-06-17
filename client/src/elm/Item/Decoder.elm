module Item.Decoder
    exposing
        ( decodeItems
        )

import Item.Model exposing (Item, ItemId, EveryDictListItems)
import DictList exposing (DictList, decodeArray2, empty)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import Utils.Json exposing (decodeEmptyArrayAs, decodeInt)


decodeItems : Decoder EveryDictListItems
decodeItems =
    oneOf
        [ decodeArray2 (field "id" decodeItemId) decodeItem
        , decodeEmptyArrayAs DictList.empty
        ]



-- decodeItemId : Decoder ItemId
-- decodeItemId =
--     decodeInt |> Decode.map ItemId


decodeItemId : Decoder ItemId
decodeItemId =
    decodeInt


decodeItem : Decoder Item
decodeItem =
    decode Item
        |> required "name" string
