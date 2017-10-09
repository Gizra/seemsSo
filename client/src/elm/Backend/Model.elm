module Backend.Model exposing (..)

{-| The `Backend` hierarchy is for code that represents entities from the
backend. It is reponsible for fetching them, saving them, etc.

  - There shouldn't be any UI code here (except possibly some UI that
    is specifically related to fetching and saving -- we'll see).

  - There shouldn't be data here that purely relates to the local state of the
    app. If it isn't persisted to the backend, that state can go elsewhere.

The nice thing about this is that we can segregate local state (like whether
a dialog box is open etc.) from the state that persists to the backend.
That way, we can more easily have a single source of truth for the
backend data -- we're not tempted to duplicate it in various places
in the UI.

-}

import Backend.Entities exposing (..)
import Backend.Restful exposing (EntityDictList)
import Backend.Item.Model exposing (Item)
import RemoteData exposing (RemoteData(..), WebData)


{-| This model basically represents things we have locally which also belong
on the backend. So, conceptually it is a kind of a local cache of some of the
things on the backend.
-}
type alias Model =
    { items : WebData (EntityDictList ItemId Item)
    }


emptyModel : Model
emptyModel =
    { items = NotAsked
    }


{-| These are all the messages related to getting things from the backend and
putting things back into the backend.
-}
type Msg
    = FetchItems
    | HandleFetchedItems (WebData (EntityDictList ItemId Item))
