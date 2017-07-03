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
    , comment =
        """## Some Markdown text

With _italic_, __bold__ and a [link](https://example.com)!
"""
    , status = NotAsked
    , selectedTab = Edit
    }


type Tab
    = Edit
    | Preview


type Msg
    = SetComment String
    | SetTab Tab
