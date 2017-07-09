module Pages.Item.Model exposing (..)

import EveryDictList
import Item.Model exposing (ItemId)
import ItemComment.Model exposing (EveryDictListItemComments)


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
    = NoOp
