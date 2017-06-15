module App.Model
    exposing
        ( emptyModel
        , Flags
        , Msg(..)
        , Model
        )

import App.Types exposing (Widget(..))
import Homepage.Model exposing (Model, Msg)
<<<<<<< HEAD
import User.Model exposing (User)


type Msg
    = HandleUser (Result String User)
    | MsgPagesHomepage Homepage.Model.Msg
=======


type Msg
    = MsgPagesHomepage Homepage.Model.Msg
>>>>>>> master


type alias Flags =
    { widget : String
    }


type alias Model =
    { widget : Widget
    , pageHomepage : Homepage.Model.Model
<<<<<<< HEAD
    , user : Maybe User
=======
>>>>>>> master
    }


emptyModel : Model
emptyModel =
    { widget = NotFound
    , pageHomepage = Homepage.Model.emptyModel
<<<<<<< HEAD
    , user = Nothing
=======
>>>>>>> master
    }
