module ItemComment.Model exposing (..)


type alias Model =
    { selectedTab : Tab
    }


type Tab
    = Edit
    | Preview


type Msg
    = SetTab Tab
