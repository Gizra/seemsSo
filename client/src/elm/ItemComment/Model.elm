module ItemComment.Model exposing (..)

import Item.Model exposing (ItemId)
import RemoteData exposing (..)


type alias Model =
    { itemId : ItemId
    , comment : String
    , status : WebData ()
    , selectedTab : Tab
    }


emptyModel : Model
emptyModel =
    { itemId = 0
    , comment = "Some text"
    , status = NotAsked
    , selectedTab = Edit
    }


type Tab
    = Edit
    | Preview


type Msg
    = SetComment String
    | SetTab Tab
