module Item.Model exposing (..)


type ItemId
    = ItemId Int


type alias Item =
    { name : String
    }


type alias DictListContact =
    EveryDictList ItemId Item
