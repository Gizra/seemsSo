module App.Model
    exposing
        ( Flags
        , Model
        , Msg(..)
        , emptyModel
        )

import App.Types exposing (BackendUrl(..), Page(..))
import Backend.Model
import Pages.Item.Model
import User.Model exposing (CurrentUser(Anonymous), User)


type Msg
    = HandleUser (Result String CurrentUser)
    | MsgBackend Backend.Model.Msg
    | MsgPagesItem Pages.Item.Model.Msg


type alias Flags =
    { page : String
    , entityId : Maybe Int
    , backendUrl : String
    }


type alias Model =
    { activePage : Page
    , backend : Backend.Model.Model
    , user : CurrentUser
    , backendUrl : BackendUrl
    , pagesItem : Pages.Item.Model.Model
    }


emptyModel : Model
emptyModel =
    { activePage = NotFound
    , backend = Backend.Model.emptyModel
    , user = Anonymous
    , backendUrl = BackendUrl ""
    , pagesItem = Pages.Item.Model.emptyModel
    }
