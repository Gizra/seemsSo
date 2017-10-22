module ItemComment.Model
    exposing
        ( DelegatedMsg(..)
        , Model
        , Msg(..)
        , Tab(..)
        , emptyModel
        )

import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Model
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
    | DelegatedSaveComment ( StorageKey ItemId, StorageKey ItemCommentId )
    | SetComment ( StorageKey ItemId, StorageKey ItemCommentId ) String


type DelegatedMsg
    = NoOp
    | MsgBackendItem Backend.Model.Msg
    | UpdateBackend
