module Item.Model exposing (..)

import DictList exposing (EveryDictList)


type ItemId
    = ItemId Int


type alias Item =
    { name : String
    }


type alias EveryDictListItems =
    EveryDictList ItemId Item
