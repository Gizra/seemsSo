module ItemComment.Model exposing (..)

import Date exposing (Date)
import EveryDictList exposing (EveryDictList)
import Http
import Item.Model exposing (ItemId)
import RemoteData exposing (..)
import User.Model exposing (User, UserId)


type ItemCommentId
    = ItemCommentId Int


type alias Model =
    { itemId : ItemId
    , comment : String
    , status : WebData ItemComment
    , selectedTab : Tab
    }


type alias ItemComment =
    { userId : UserId
    , user : User
    , comment : String
    , created : Date
    }


type alias EveryDictListItemComments =
    EveryDictList ItemCommentId ItemComment


emptyModel : Model
emptyModel =
    { itemId = 0
    , comment = ""
    , status = NotAsked
    , selectedTab = Edit
    }


type Tab
    = Edit
    | Preview


type Msg
    = HandleSaveComment (Result Http.Error EveryDictListItemComments)
    | SaveComment
    | SetComment String
    | SetTab Tab
