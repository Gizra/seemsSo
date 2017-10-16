module Pages.Item.Model
    exposing
        ( Model
        , Msg(..)
        , emptyModel
        )

import Backend.Entities exposing (ItemCommentId)
import Backend.Model
import ItemComment.Model exposing (Model, Msg(..))
import StorageKey exposing (StorageKey)


type alias Model =
    { itemComment : ItemComment.Model.Model
    }


emptyModel : Model
emptyModel =
    { itemComment = ItemComment.Model.emptyModel
    }


type Msg
    = MsgBackendItem Backend.Model.Msg
    | MsgItemComment ItemComment.Model.Msg
    | SetComment (StorageKey ItemCommentId) String
