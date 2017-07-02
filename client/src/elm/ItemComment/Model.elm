module ItemComment.Model exposing (..)

import Item.Model exposing (ItemId)
import RemoteData exposing (WebData)


type alias ItemComment =
    { itemId : ItemId
    , comment : String
    , status : WebData ()
    }
