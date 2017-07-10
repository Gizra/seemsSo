module Pages.Homepage.Model exposing (..)

import DictList exposing (DictList)
import Item.Model exposing (EveryDictListItems)


type alias Model =
    { items : EveryDictListItems
    }


emptyModel : Model
emptyModel =
    { items = DictList.empty
    }


type Msg
    = HandleItems (Result String EveryDictListItems)
