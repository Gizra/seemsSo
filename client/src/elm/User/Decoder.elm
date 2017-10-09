module User.Decoder
    exposing
        ( decodeCurrentUser
        , decodeUserTuple
        )

import Backend.Entities exposing (UserId)
import Backend.Restful exposing (EntityId, decodeId, toEntityId)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, null, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)
import User.Model exposing (CurrentUser(..), User, UserTuple)
import Utils.Json exposing (decodeInt)


decodeCurrentUser : Decoder CurrentUser
decodeCurrentUser =
    oneOf
        [ null Anonymous
        , decodeUserTuple |> andThen (\userTuple -> succeed <| Authenticated userTuple)
        ]


decodeUserTuple : Decoder UserTuple
decodeUserTuple =
    decode (,)
        |> custom (decodeId toEntityId)
        |> custom decodeUser


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "name" string
