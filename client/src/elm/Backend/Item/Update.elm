port module Backend.Item.Update
    exposing
        ( subscriptions
        , update
        )

import App.Types exposing (BackendUrl(..))
import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Decoder exposing (decodeItemComments, decodeItems, deocdeItemIdAndComments)
import Backend.Item.Model exposing (ItemComment, Msg(..))
import Backend.Item.Utils exposing (getComment, insertComments)
import Backend.Model exposing (Model)
import Backend.Restful exposing (fromEntityId)
import Editable
import Editable.WebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)
import HttpBuilder exposing (send, toTask, withCredentials, withExpect, withJsonBody, withQueryParams)
import Json.Decode exposing (Value, decodeValue)
import Json.Encode exposing (object, string)
import Maybe.Extra exposing (unwrap)
import StorageKey exposing (StorageKey)
import User.Model exposing (CurrentUser)
import Utils.WebData exposing (sendWithHandler)


{-| This is a delegated update for bundling related `Msg`s together.

@todo: Clarify Model is of Backend, and Msg is of current model.

-}
update : BackendUrl -> CurrentUser -> Msg -> Model -> ( Model, Cmd Msg )
update backendUrl currentUser msg model =
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

        HandleFetchItemIdAndCommentsTuple (Ok ( storageKey, comments )) ->
            let
                itemsUpdated =
                    unwrap model.items
                        (\item ->
                            let
                                itemUpdated =
                                    { item | comments = comments }
                            in
                            EveryDictList.insert storageKey itemUpdated model.items
                        )
                        (EveryDictList.get storageKey model.items)
            in
            ( { model | items = itemsUpdated }
            , Cmd.none
            )

        HandleFetchItemIdAndCommentsTuple (Err error) ->
            let
                _ =
                    Debug.log "HandleFetchItemIdAndCommentsTuple" error
            in
            model ! []

        SaveComment storageKeys ->
            case getComment storageKeys model.items of
                Nothing ->
                    ( model, Cmd.none )

                Just itemComment ->
                    ( model
                    , saveComment backendUrl currentUser storageKeys itemComment
                    )

        HandleSaveComment storageKeys (Ok itemComments) ->
            ( { model | items = insertComments storageKeys itemComments model.items }
            , Cmd.none
            )

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


saveComment : BackendUrl -> CurrentUser -> ( StorageKey ItemId, StorageKey ItemCommentId ) -> EditableWebData ItemComment -> Cmd Msg
saveComment (BackendUrl backendUrl) currentUser storageKeys editableWebData =
    let
        itemId =
            Tuple.first storageKeys
                |> StorageKey.value
                |> Maybe.map (fromEntityId >> toString)
                |> Maybe.withDefault ""

        itemComment =
            editableWebData
                |> Editable.WebData.toEditable
                |> Editable.value
    in
    HttpBuilder.post (backendUrl ++ "/api/comments/" ++ itemId)
        |> withCredentials
        |> withQueryParams [ ( "_accept", "application/json" ) ]
        |> withJsonBody (object [ ( "comment", string itemComment.comment ) ])
        |> sendWithHandler (decodeItemComments currentUser) (HandleSaveComment storageKeys)



-- PORTS & Subscriptions


port items : (Value -> msg) -> Sub msg


port itemIdAndCommentsTuple : (Value -> msg) -> Sub msg


subscriptions : CurrentUser -> Sub Msg
subscriptions currentUser =
    Sub.batch
        [ items (decodeValue (decodeItems currentUser) >> HandleFetchItems)
        , itemIdAndCommentsTuple (decodeValue (deocdeItemIdAndComments currentUser) >> HandleFetchItemIdAndCommentsTuple)
        ]
