port module Backend.Item.Update
    exposing
        ( subscriptions
        , update
        )

import App.Types exposing (BackendUrl)
import Backend.Item.Decoder exposing (decodeItems)
import Backend.Item.Model exposing (Msg(..))
import Backend.Model exposing (Model)
import EveryDictList
import Json.Decode exposing (Value, decodeValue)


{-| This is a delegated update for bundling related `Msg`s together.

@todo: Clarify Model is of Backend, and Msg is of current model.

-}
update : BackendUrl -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl msg model =
    case msg of
        HandleFetchItems (Ok items) ->
            ( { model | items = items }
            , Cmd.none
            )

        HandleFetchItems (Err error) ->
            let
                _ =
                    Debug.log "HandleItems" err
            in
                model ! []

        SaveComment ( itemId, storageKey ) ->
            model ! Cmd.none

        -- ( { model | status = Loading }
        -- , saveComment model
        -- , Nothing
        -- )
        HandleSaveComment ( itemId, storageKey ) (Ok everyDictListItemComments) ->
            model ! []

        -- ( { model | comment = "", status = NotAsked }
        -- , Cmd.none
        -- , Just everyDictListItemComments
        -- )
        HandleSaveComment ( itemId, storageKey ) (Err err) ->
            model ! []



-- let
--     _ =
--         Debug.log "HandleSaveComment (Err)" False
-- in
--     ( { model | status = Failure err }
--     , Cmd.none
--     , Nothing
--     )


saveComment : BackendUrl -> ( ItemId, StorageKey ItemCommentId ) -> ItemComment -> Cmd Msg
saveComment backendUrl ( itemId, storageKey ) itemComment =
    HttpBuilder.post (backendUrl ++ "/api/comments/" ++ (toString itemId))
        |> withCredentials
        |> withQueryParams [ ( "_accept", "application/json" ) ]
        |> withJsonBody (object [ ( "comment", string itemComment.comment ) ])
        |> sendWithHandler decodeItemComments (HandleSaveComment ( itemId, storageKey ))



-- PORTS & Subscriptions


port items : (Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    items (decodeValue decodeItems >> HandleItems)
