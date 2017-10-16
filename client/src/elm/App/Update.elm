port module App.Update
    exposing
        ( init
        , subscriptions
        , update
        )

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Backend.Restful exposing (toEntityId)
import Backend.Update
import EveryDictList
import Json.Decode exposing (Value, decodeValue)
import Pages.Item.Model
import Pages.Item.Update
import StorageKey exposing (StorageKey(Existing))
import Task
import User.Decoder exposing (decodeCurrentUser)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        page =
            case flags.page of
                "item" ->
                    -- @todo: Get the item id.
                    toEntityId 1
                        |> Existing
                        |> Item

                "homepage" ->
                    HomePage

                -- Fallback to page not found.
                _ ->
                    NotFound
    in
    ( { emptyModel | activePage = page }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleUser (Ok user) ->
            { model | user = user } ! []

        HandleUser (Err err) ->
            let
                _ =
                    Debug.log "HandleUser" err
            in
            model ! []

        MsgBackend subMsg ->
            let
                ( subModel, subCmds ) =
                    Backend.Update.update model.backendUrl subMsg model.backend
            in
            ( { model | backend = subModel }
            , Cmd.map MsgBackend subCmds
            )

        MsgPagesItem subMsg ->
            let
                noBackendChange =
                    ( model.backend, Cmd.none )

                ( subModel, subCmds, ( backendUpdated, backendCmds ) ) =
                    case model.activePage of
                        Item itemId ->
                            let
                                ( subModel, subCmds, ( partialBackendModel, delegatedMsg ) ) =
                                    Pages.Item.Update.update model.backendUrl subMsg model.pagesItem ( itemId, model.backend )
                            in
                            case delegatedMsg of
                                Pages.Item.Model.NoOp ->
                                    -- No change to the backend.
                                    -- Make the return value a Maybe, just with the changed values?
                                    ( subModel, subCmds, ( model.backend, Cmd.none ) )

                                Pages.Item.Model.MsgBackendItem backendMsg ->
                                    let
                                        -- @todo: Call Pages.Item.Model.UpdateBackend to remove duplication?
                                        backend =
                                            model.backend

                                        backendUpdated =
                                            { backend | items = EveryDictList.union partialBackendModel.items model.backend.items }

                                        msg =
                                            backendMsg
                                                |> Task.succeed
                                                |> Task.perform identity
                                                |> Cmd.map MsgBackend
                                    in
                                    ( subModel, subCmds, ( backendUpdated, msg ) )

                                -- Just update the backend data, but without
                                -- any Cmd. This will be used when a sub-model will edit the data (e.g. via a form)
                                -- but the form was not submitted yet.
                                Pages.Item.Model.UpdateBackend ->
                                    let
                                        backend =
                                            model.backend

                                        backendUpdated =
                                            { backend | items = EveryDictList.union partialBackendModel.items model.backend.items }
                                    in
                                    ( subModel, subCmds, ( backendUpdated, Cmd.none ) )

                        _ ->
                            ( model.pagesItem, Cmd.none, ( model.backend, Cmd.none ) )
            in
            ( { model | pagesItem = subModel }
            , Cmd.batch
                [ Cmd.map MsgPagesItem subCmds
                , backendCmds
                ]
            )



-- MsgPagesHomepage subMsg ->
--     let
--         ( val, cmds ) =
--             Pages.Homepage.Update.update subMsg model.pageHomepage
--     in
--         ( { model | pageHomepage = val }
--         , Cmd.map MsgPagesHomepage cmds
--         )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            case model.activePage of
                Item itemId ->
                    Sub.map MsgBackend <| Backend.Update.subscriptions model.user

                HomePage ->
                    Sub.map MsgBackend <| Backend.Update.subscriptions model.user

                NotFound ->
                    Sub.none
    in
    Sub.batch
        [ user (decodeValue decodeCurrentUser >> HandleUser)
        , subs
        ]



-- PORTS


port user : (Value -> msg) -> Sub msg
