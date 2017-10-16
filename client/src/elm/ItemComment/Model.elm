module ItemComment.Model
    exposing
        ( DelegatedMsg(..)
        , Model
        , Msg(..)
        , Tab(..)
        , emptyModel
        )

import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Model
import StorageKey exposing (StorageKey)


type alias Model =
    { selectedTab : Tab
    }


emptyModel : Model
emptyModel =
    { selectedTab = Edit
    }


type Tab
    = Edit
    | Preview


type Msg
    = SetTab Tab
      -- Coresponds to`SaveComment` in `Backend.Item.Model`
    | DelegatedSaveComment ( StorageKey ItemId, StorageKey ItemCommentId )


type DelegatedMsg
    = NoOp
    | MsgBackendItem Backend.Item.Model.Msg
