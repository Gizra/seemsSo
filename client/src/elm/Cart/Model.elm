module Cart.Model exposing (..)

import DictList exposing (DictList)
import Item.Model exposing (EveryDictListItems, ItemId)


type alias Model =
    { items : List ItemId
    }


emptyModel : Model
emptyModel =
    { items = []
    }


type Msg
    = AddItem (Result String ItemId)
    | ClearCart
    | RemoveItem (Result String ItemId)
