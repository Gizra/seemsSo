module App.Model
    exposing
        ( emptyModel
        , Flags
        , Msg(..)
        , Model
        )

import App.Types exposing (Widget(..))
import Backend.Item.Model exposing (Item)
import Backend.Restful exposing (EntityDictList)
import Pages.Homepage.Model exposing (Model, Msg)
import Pages.Item.Model exposing (Model, Msg)
import User.Model exposing (CurrentUser(Anonymous), User)


type Msg
    = HandleUser (Result String (Maybe User))
    | MsgPagesHomepage Pages.Homepage.Model.Msg
    | MsgPagesItem Pages.Item.Model.Msg


type alias Flags =
    { page : String
    }


type alias Model =
    { activePage : Page
    , items : EntityDictList ItemId Item
    , user : CurrentUser
    , baseUrl : String
    }


emptyModel : Model
emptyModel =
    { activePage = NotFound
    , items = EveryDictList.empty
    , user = Anonymous

    -- @todo: Get dynamically.
    , baseUrl =
        "http://localhost:3000/"
    }
