module App.Model
    exposing
        ( Flags
        , Model
        , Msg(..)
        , emptyModel
        )

import App.Types exposing (BackendUrl(..), Page(..))
import Backend.Model
import User.Model exposing (CurrentUser(Anonymous), User)


type Msg
    = HandleUser (Result String CurrentUser)
    | MsgBackend Backend.Model.Msg



-- | MsgPagesHomepage
-- | MsgPagesItem


type alias Flags =
    { page : String
    }


type alias Model =
    { activePage : Page
    , backend : Backend.Model.Model
    , user : CurrentUser
    , backendUrl : BackendUrl
    }


emptyModel : Model
emptyModel =
    { activePage = NotFound
    , backend = Backend.Model.emptyModel
    , user = Anonymous

    -- @todo: Get dynamically.
    , backendUrl = BackendUrl "http://localhost:3000/"
    }
