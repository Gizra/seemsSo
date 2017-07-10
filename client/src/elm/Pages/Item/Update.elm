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
    items (decodeValue deocdeItemIdAndComments >> HandleItems)



-- PORTS


port itemIdAndComments : (Value -> msg) -> Sub msg
