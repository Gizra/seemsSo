module Pages.Item.Model exposing (..)

import Item.Model exposing (ItemId)
import ItemComment.Model


type alias Model =
    { itemId : ItemId
    , itemComment : ItemComment.Model.Model
    }


emptyModel : Model
emptyModel =
    { itemId = 0
    , itemComment = ItemComment.Model.emptyModel
    }
