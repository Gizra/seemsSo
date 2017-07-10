module Pages.Item.Model exposing (..)

import Date
import EveryDictList
import Item.Model exposing (ItemId)
import ItemComment.Model exposing (EveryDictListItemComments, ItemCommentId)
import User.Model exposing (User)


type alias Model =
    { itemId : ItemId
    , comments : EveryDictListItemComments
    , itemComment : ItemComment.Model.Model
    }


emptyModel : Model
emptyModel =
    { itemId = 0
    , comments = EveryDictList.empty
    , itemComment = ItemComment.Model.emptyModel
    }


type Msg
    = HandleItemIdAndComments (Result String ( ItemId, EveryDictListItemComments ))
    | MsgItemComment ItemComment.Model.Msg
