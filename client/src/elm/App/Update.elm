port module App.Update
    exposing
        ( init
        , update
        , subscriptions
        )

import App.Model exposing (..)
import App.Types exposing (Widget(..))
import Pages.Homepage.Update
import Pages.Item.Update
import Json.Decode exposing (Value, decodeValue)
import User.Decoder exposing (decodeMaybeUser)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        widget =
            case flags.widget of
                "item" ->
                    Item

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
                    Pages.Homepage.Update.update subMsg model.pageHomepage
            in
                ( { model | pageHomepage = val }
                , Cmd.map MsgPagesHomepage cmds
                )

        MsgPagesItem subMsg ->
            let
                ( val, cmds ) =
                    Pages.Item.Update.update subMsg model.pageItem
            in
                ( { model | pageItem = val }
                , Cmd.map MsgPagesItem cmds
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            case model.widget of
                Item ->
                    -- @todo: Subscribe to get items and comments.
                    Sub.none

                HomePage ->
                    Sub.map MsgPagesHomepage <| Pages.Homepage.Update.subscriptions

                NotFound ->
                    Sub.none
    in
        Sub.batch
            [ user (decodeValue decodeMaybeUser >> HandleUser)
            , subs
            ]



-- PORTS


port user : (Value -> msg) -> Sub msg
