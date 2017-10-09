module App.Model
    exposing
        ( emptyModel
        , Flags
        , Msg(..)
        , Model
        )

import App.Types exposing (Page(..))
import Backend.Item.Model exposing (Item)
import Backend.Model
import Backend.Restful exposing (EntityDictList)
import User.Model exposing (CurrentUser(Anonymous), User)


type Msg
    = HandleUser (Result String CurrentUser)



-- | MsgPagesHomepage
-- | MsgPagesItem


type alias Flags =
    { page : String
    }


type alias Model =
    { activePage : Page
    , backend : Backend.Model.Model
    , user : CurrentUser
    , baseUrl : String
    }


emptyModel : Model
emptyModel =
    { activePage = NotFound
    , backend = Backend.Model.emptyModel
    , user = Anonymous

    -- @todo: Get dynamically.
    , baseUrl =
        "http://localhost:3000/"
    }
