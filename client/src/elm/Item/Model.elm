module Item.Model exposing (..)

import DictList exposing (DictList)


-- @todo: Will be used with EveryDictList
-- type ItemId
--     = ItemId Int


type alias ItemId =
    Int


type alias Item =
    { name : String
    }


type alias EveryDictListItems =
    DictList ItemId Item
