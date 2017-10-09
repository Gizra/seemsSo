module Backend.Item.Model exposing (..)

import Backend.Entities exposing (ItemCommentId)
import Backend.Restful exposing (EntityDictList)
import Backend.ItemComment.Model exposing (ItemComment)
import Editable.WebData


type alias Item =
    { name : String
    , comments : EntityDictList ItemCommentId (EditableWebData ItemComment)
    }
