module User.Model
    exposing
        ( CurrentUser(..)
        , User
        , UserTuple
        )

import Backend.Entities exposing (UserId)


type CurrentUser
    = Anonymous
    | Authenticated UserTuple


type alias User =
    { name : String
    }


type alias UserTuple =
    ( UserId, User )
