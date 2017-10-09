port module ItemComment.Update
    exposing
        ( update
        )

import HttpBuilder exposing (send, toTask, withCredentials, withExpect, withJsonBody, withQueryParams)
import ItemComment.Decoder exposing (decodeEveryDictListItemComments)
import ItemComment.Model exposing (EveryDictListItemComments, ItemComment, ItemCommentId, Model, Msg(..))
import Json.Encode exposing (object, string)
import RemoteData exposing (..)
import Utils.WebData exposing (sendWithHandler)


update : Msg -> Model -> ( Model, Cmd Msg, Maybe EveryDictListItemComments )
update msg model =
    case msg of
        SaveComment ->
            ( { model | status = Loading }
            , saveComment model
            , Nothing
            )

        HandleSaveComment (Ok everyDictListItemComments) ->
            ( { model | comment = "", status = NotAsked }
            , Cmd.none
            , Just everyDictListItemComments
            )

        HandleSaveComment (Err err) ->
            let
                _ =
                    Debug.log "HandleSaveComment (Err)" False
            in
                ( { model | status = Failure err }
                , Cmd.none
                , Nothing
                )

        SetComment comment ->
            ( { model | comment = comment }
            , Cmd.none
            , Nothing
            )

        SetTab tab ->
            ( { model | selectedTab = tab }
            , Cmd.none
            , Nothing
            )


saveComment : Model -> Cmd Msg
saveComment model =
    let
        -- @todo: Change.
        backendUrl =
            "http://localhost:3000"
    in
        HttpBuilder.post (backendUrl ++ "/api/comments/" ++ (toString model.itemId))
            |> withCredentials
            |> withQueryParams [ ( "_accept", "application/json" ) ]
            |> withJsonBody (object [ ( "comment", string model.comment ) ])
            |> sendWithHandler decodeEveryDictListItemComments HandleSaveComment
