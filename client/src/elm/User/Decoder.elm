module User.Decoder
    exposing
        ( decodeUser
        )

import User.Model exposing (User)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, float, index, int, keyValuePairs, list, map, map2, null, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, optional, optionalAt, required, requiredAt)


decodeUser : Decoder (Maybe User)
decodeUser =
    oneOf
        [ null Nothing
        , decode User
            |> required "name" string
            |> andThen (\user -> succeed <| Just user)
        ]
