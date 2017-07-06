port module App.Update
    exposing
        ( init
        , update
        , subscriptions
        )

import App.Model exposing (..)
import App.Types exposing (Widget(..))
import Homepage.Update
import ItemComment.Update
import Json.Decode exposing (Value, decodeValue)
import User.Decoder exposing (decodeUser)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        widget =
            case flags.widget of
                "itemComment" ->
                    ItemComment

                "homepage" ->
                    HomePage

                -- Fallback to page not found.
                _ ->
                    NotFound
    in
        ( { emptyModel | widget = widget }
        , Cmd.none
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleUser (Ok muser) ->
            { model | user = muser } ! []

        HandleUser (Err err) ->
            let
                _ =
                    Debug.log "HandleUser" err
            in
                model ! []

        MsgPagesHomepage subMsg ->
            let
                ( val, cmds ) =
                    Homepage.Update.update subMsg model.pageHomepage
            in
                ( { model | pageHomepage = val }
                , Cmd.map MsgPagesHomepage cmds
                )

        MsgPagesItemComment subMsg ->
            let
                ( val, cmds ) =
                    ItemComment.Update.update subMsg model.pageItemComment
            in
                ( { model | pageItemComment = val }
                , Cmd.map MsgPagesItemComment cmds
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            case model.widget of
                ItemComment ->
                    Sub.none

                HomePage ->
                    Sub.map MsgPagesHomepage <| Homepage.Update.subscriptions

                NotFound ->
                    Sub.none
    in
        Sub.batch
            [ user (decodeValue decodeUser >> HandleUser)
            , subs
            ]



-- PORTS


port user : (Value -> msg) -> Sub msg
