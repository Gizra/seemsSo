module App.Model
    exposing
        ( emptyModel
        , Flags
        , Msg(..)
        , Model
        )

import App.Types exposing (Widget(..))
import Pages.Homepage.Model exposing (Model, Msg)
import Pages.Item.Model exposing (Model, Msg)
import User.Model exposing (User)


type Msg
    = HandleUser (Result String (Maybe User))
    | MsgPagesHomepage Pages.Homepage.Model.Msg
    | MsgPagesItem Pages.Item.Model.Msg


type alias Flags =
    { widget : String
    }


type alias Model =
    { widget : Widget
    , pageHomepage : Pages.Homepage.Model.Model
    , pageItem : Pages.Item.Model.Model
    , user : Maybe User
    , baseUrl : String
    }


emptyModel : Model
emptyModel =
    { widget = NotFound
    , pageHomepage = Pages.Homepage.Model.emptyModel
    , pageItem = Pages.Item.Model.emptyModel
    , user = Nothing

    -- @todo: Get dynamically.
    , baseUrl =
        "http://localhost:3000/"
    }
