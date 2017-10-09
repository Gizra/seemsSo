port module Backend.Item.Update
    exposing
        ( subscriptions
        , update
        )

import App.Types exposing (BackendUrl(..))
import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Decoder exposing (decodeItemComments, decodeItems)
import Backend.Item.Model exposing (ItemComment, Msg(..))
import Backend.Model exposing (Model)
import HttpBuilder exposing (send, toTask, withCredentials, withExpect, withJsonBody, withQueryParams)
import Json.Decode exposing (Value, decodeValue)
import Json.Encode exposing (object, string)
import StorageKey exposing (StorageKey)
import Utils.WebData exposing (sendWithHandler)


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
                    Debug.log "HandleItems" error
            in
            model ! []

        SaveComment ( itemId, storageKey ) ->
            model ! []

        -- ( { model | status = Loading }
        -- , saveComment model
        -- , Nothing
        -- )
        HandleSaveComment ( itemId, storageKey ) (Ok itemComment) ->
            model ! []

        -- ( { model | comment = "", status = NotAsked }
        -- , Cmd.none
        -- , Just everyDictListItemComments
        -- )
        HandleSaveComment ( itemId, storageKey ) (Err error) ->
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
saveComment (BackendUrl backendUrl) ( itemId, storageKey ) itemComment =
    HttpBuilder.post (backendUrl ++ "/api/comments/" ++ toString itemId)
        |> withCredentials
        |> withQueryParams [ ( "_accept", "application/json" ) ]
        |> withJsonBody (object [ ( "comment", string itemComment.comment ) ])
        |> sendWithHandler decodeItemComments (HandleSaveComment ( itemId, storageKey ))



-- PORTS & Subscriptions


port items : (Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    items (decodeValue decodeItems >> HandleFetchItems)
