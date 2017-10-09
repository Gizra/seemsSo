module ItemComment.Model exposing (..)

import Date exposing (Date)
import EveryDictList exposing (EveryDictList)
import Http
import RemoteData exposing (..)
import User.Model exposing (User, UserId)


type alias ItemComment =
    { user : UserId
    , user : User
    , comment : String
    , created : Date
    }


emptyModel : Model
emptyModel =
    { itemId = 0
    , comment = ""
    , status = NotAsked
    }


type Msg
    = HandleSaveComment (Result Http.Error EveryDictListItemComments)
    | SaveComment
    | SetComment String
