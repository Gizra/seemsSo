module User.Model exposing (..)

import Backend.Entities exposing (UserId)
import RemoteData exposing (WebData)


type AccessToken
    = AccessToken String


{-| While we are fetching the user's info we are still Anonymous.
Once we got all the info, we'll change the type to `Authenticated`
-}
type Me
    = Anonymous (WebData AccessToken) (WebData User)
    | Authenticated AccessToken ( UserId, User )


type alias User =
    { name : String
    }
