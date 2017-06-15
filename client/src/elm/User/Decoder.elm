module User.Decoder
    exposing
        ( decodeUser
        )

import User.Model exposing (User)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import Utils.Json exposing (decodeEmptyArrayAs, decodeInt)


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "name" string
