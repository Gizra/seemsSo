port module Pages.Item.Update
    exposing
        ( update
        )

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
            ( { model
                | itemId = itemId
                , comments = everyDictListItemComments
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
                ( val, cmds ) =
                    ItemComment.Update.update subMsg model.itemComment
            in
                ( { model | itemComment = val }
                , Cmd.map MsgItemComment cmds
                )


subscriptions : Sub Msg
subscriptions =
    itemIdAndComments (decodeValue deocdeItemIdAndComments >> HandleItemIdAndComments)



-- PORTS


port itemIdAndComments : (Value -> msg) -> Sub msg
