module ItemComment.Model exposing (..)

import Item.Model exposing (ItemId)
import RemoteData exposing (..)


type alias Model =
    { itemId : ItemId
    , comment : String
    , status : WebData ()
    }


emptyModel : Model
emptyModel =
    { itemId = 0
    , comment = ""
    , status = NotAsked
    }


type Msg
    = NoOp
