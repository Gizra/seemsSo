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
import Json.Decode exposing (Value, decodeValue)
import Pages.Item.Update
import User.Decoder exposing (decodeCurrentUser)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        page =
            case flags.page of
                "item" ->
                    -- @todo: Get the item id.
                    Item (toEntityId 1)

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
                ( subModel, subCmds ) =
                    case model.activePage of
                        Item itemId ->
                            Pages.Item.Update.update subMsg model.pagesItem model.backend.items itemId

                        _ ->
                            ( model.pagesItem, Cmd.none )
            in
            ( { model | pagesItem = subModel }
            , Cmd.map MsgPagesItem subCmds
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
                    Sub.map MsgBackend <| Backend.Update.subscriptions

                HomePage ->
                    Sub.map MsgBackend <| Backend.Update.subscriptions

                NotFound ->
                    Sub.none
    in
    Sub.batch
        [ user (decodeValue decodeCurrentUser >> HandleUser)
        , subs
        ]



-- PORTS


port user : (Value -> msg) -> Sub msg
