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
    , comments =
        EveryDictList.fromList
            ([ ( ItemComment.Model.ItemCommentId 1
               , { userId = 100
                 , user = User "alice"
                 , comment = "Comment #1"
                 , created = Date.fromTime 28347887
                 }
               )
             , ( ItemComment.Model.ItemCommentId 2
               , { userId = 200
                 , user = User "bob"
                 , comment = "Comment #2"
                 , created = Date.fromTime 28347897
                 }
               )
             ]
            )
    , itemComment = ItemComment.Model.emptyModel
    }


type Msg
    = HandleItemIdAndComments (Result String ( ItemId, EveryDictListItemComments ))
    | MsgItemComment ItemComment.Model.Msg
