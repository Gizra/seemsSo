module Backend.Item.Model
    exposing
        ( Company
        , Item
        , ItemComment
        , Msg(..)
        )

import Amount exposing (Amount)
import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Restful exposing (EntityDictList)
import Date exposing (Date)
import Editable.WebData exposing (EditableWebData)
import Http
import StorageKey exposing (StorageKey)
import User.Model exposing (UserTuple)


type alias Item =
    { name : String
    , comments : EntityDictList ItemCommentId (EditableWebData ItemComment)
    , price : Amount
    , company : Company
    }


type alias Company =
    { name : String
    }


type alias ItemComment =
    { user : UserTuple
    , comment : String
    , created : Date
    }


type Msg
    = HandleFetchItems (Result String (EntityDictList ItemId Item))
    | HandleFetchItemIdAndCommentsTuple (Result String ( StorageKey ItemId, EntityDictList ItemCommentId (EditableWebData ItemComment) ))
    | SaveComment ( StorageKey ItemId, StorageKey ItemCommentId )
    | HandleSaveComment ( StorageKey ItemId, StorageKey ItemCommentId ) (Result Http.Error (EntityDictList ItemCommentId (EditableWebData ItemComment)))
