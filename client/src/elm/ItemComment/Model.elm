module ItemComment.Model
    exposing
        ( Model
        , Msg(..)
        , Tab(..)
        , emptyModel
        )


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
