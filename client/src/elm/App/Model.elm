module App.Model
    exposing
        ( emptyModel
        , Flags
        , Msg(..)
        , Model
        )

import App.Types exposing (Widget(..))
import Homepage.Model exposing (Model, Msg)
import ItemComment.Model exposing (Model, Msg)
import User.Model exposing (User)


type Msg
    = HandleUser (Result String (Maybe User))
    | MsgPagesHomepage Homepage.Model.Msg
    | MsgPagesItemComment ItemComment.Model.Msg


type alias Flags =
    { widget : String
    }


type alias Model =
    { widget : Widget
    , pageHomepage : Homepage.Model.Model
    , pageItemComment : ItemComment.Model.Model
    , user : Maybe User
    }


emptyModel : Model
emptyModel =
    { widget = NotFound
    , pageHomepage = Homepage.Model.emptyModel
    , pageItemComment = ItemComment.Model.emptyModel
    , user = Nothing
    }
