module Backend.Update exposing (update)

{-| This could perhaps be distributed one level down, to
`Backend.Session.Update`, `Backend.Clinic.Update` etc. Or, perhaps it is nicer
to keep it together here for now.
-}

import Backend.Entities exposing (..)
import Backend.Model exposing (..)
import Backend.Restful exposing (EndPoint, toEntityId)
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Backend.Item.Model exposing (Item)
import Maybe.Extra exposing (toList)
import RemoteData exposing (RemoteData(..))


itemEndpoint : EndPoint Error () ItemId Item
itemEndpoint =
    { path = "api/items"
    , tag = toEntityId
    , decoder = decodeClinic
    , error = identity
    , params = always []
    }


update : BackendUrl -> String -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl accessToken msg model =
    let
        getFromBackend =
            -- Partially apply the backendUrl and accessToken, just for fun
            Backend.Restful.get backendUrl (Just accessToken)
    in
        case msg of
            HandleFetchedItems items ->
                ( { model | items = items }
                , Cmd.none
                )
