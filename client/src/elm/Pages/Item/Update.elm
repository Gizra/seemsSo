port module Pages.Item.Update
    exposing
        ( subscriptions
        , update
        )

import EveryDictList
import Item.Decoder exposing (decodeItems)
import ItemComment.Model exposing (ItemComment)
import ItemComment.Update
import Json.Decode exposing (Value, decodeValue)
import Pages.Item.Decoder exposing (deocdeItemIdAndComments)
import Pages.Item.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleItemIdAndComments (Ok ( itemId, everyDictListItemComments )) ->
            let
                itemComment =
                    model.itemComment

                itemCommentUpdated =
                    { itemComment | itemId = itemId }
            in
                ( { model
                    | itemId = itemId
                    , comments = everyDictListItemComments
                    , itemComment = itemCommentUpdated
                  }
                , Cmd.none
                )

        HandleItemIdAndComments (Err err) ->
            let
                _ =
                    Debug.log "HandleItemIdAndComments" err
            in
                model ! []

        MsgItemComment subMsg ->
            let
                ( val, cmds, maybeEveryDictListItemComments ) =
                    ItemComment.Update.update subMsg model.itemComment

                commentsUpdated =
                    Maybe.map (\newItemComments -> EveryDictList.union model.comments newItemComments) maybeEveryDictListItemComments
                        |> Maybe.withDefault model.comments
            in
                ( { model
                    | itemComment = val
                    , comments = commentsUpdated
                  }
                , Cmd.map MsgItemComment cmds
                )


subscriptions : Sub Msg
subscriptions =
    itemIdAndComments (decodeValue deocdeItemIdAndComments >> HandleItemIdAndComments)



-- PORTS


port itemIdAndComments : (Value -> msg) -> Sub msg
