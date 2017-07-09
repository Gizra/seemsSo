module Pages.Item.Model exposing (..)

import EveryDictList
import Item.Model exposing (ItemId)
import ItemComment.Model exposing (EveryDictListItemComments, ItemCommentId)


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
                 , comment = "Comment #1"
                 }
               )
             , ( ItemComment.Model.ItemCommentId 2
               , { userId = 200
                 , comment = "Comment #2"
                 }
               )
             ]
            )
    , itemComment = ItemComment.Model.emptyModel
    }


type Msg
    = NoOp
