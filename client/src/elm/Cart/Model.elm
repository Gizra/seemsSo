module Cart.Model exposing (..)

import Dict exposing (Dict)
import DictList exposing (DictList)
import Item.Model exposing (EveryDictListItems, Item, ItemId)
import RemoteData exposing (WebData)


type alias OrderItemId =
    Int


type alias Model =
    { items : EveryDictListItems
    , orderItems : Dict ItemId (WebData OrderItemId)
    }


emptyModel : Model
emptyModel =
    { items = DictList.empty
    , orderItems = Dict.empty
    }


type Msg
    = AddItem (Result String ( ItemId, Item ))
    | ClearCart
    | RemoveItem (Result String ItemId)
