module User.Decoder
    exposing
        ( decodeMaybeUser
        , decodeUser
        , decoderUserId
        )

import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, null, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import User.Model exposing (User, UserId)
import Utils.Json exposing (decodeInt)


decodeMaybeUser : Decoder (Maybe User)
decodeMaybeUser =
    oneOf
        [ null Nothing
        , decodeUser |> andThen (\user -> succeed <| Just user)
        ]


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "name" string


decoderUserId : Decoder UserId
decoderUserId =
    decodeInt
