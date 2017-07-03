port module ItemComment.Update
    exposing
        ( update
        )

import HttpBuilder exposing (send, toTask, withCredentials, withExpect, withJsonBody, withQueryParams)
import ItemComment.Model exposing (Model, Msg(..))
import Json.Encode exposing (string)
import RemoteData exposing (..)
import Task exposing (Task)
import Utils.WebData exposing (sendWithHandler)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveComment ->
            ( { model | status = Loading }
            , saveComment model
            )

        HandleSaveComment (Ok ()) ->
            model ! []

        HandleSaveComment (Err err) ->
            model ! []

        SetComment comment ->
            { model | comment = comment } ! []

        SetTab tab ->
            { model | selectedTab = tab } ! []


saveComment : Model -> Cmd Msg
saveComment model =
    let
        -- @todo: Change.
        backendUrl =
            "http://localhost:3000"
    in
        HttpBuilder.post (backendUrl ++ "/api/bids")
            |> withCredentials
            |> withJsonBody (string model.comment)
            |> send HandleSaveComment
