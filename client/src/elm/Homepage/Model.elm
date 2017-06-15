module Homepage.Model exposing (..)

import Date exposing (Day)
import DictList exposing (EveryDictList)
import Item.Model exposing (EveryDictListItems)


type alias Model =
    { items : EveryDictListItems
    }


emptyModel : Model
emptyModel =
    { items = EveryDictList.empty
    }


type Msg
    = HandleItems (Result String DictListItems)
