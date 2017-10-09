module User.Model
    exposing
        ( CurrentUser(..)
        , User
        , UserTuple
        )

import Backend.Entities exposing (UserId)
import RemoteData exposing (WebData)


type CurrentUser
    = Anonymous
    | Authenticated UserTuple


type alias User =
    { name : String
    }


type alias UserTuple =
    ( UserId, User )
