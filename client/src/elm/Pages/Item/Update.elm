port module Pages.Item.Update
    exposing
        ( update
        )

import Item.Decoder exposing (decodeItems)
import Pages.Item.Model exposing (Model, Msg(..))
import Json.Decode exposing (Value, decodeValue)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []



--
-- subscriptions : Sub Msg
-- subscriptions =
--     items (decodeValue decodeItems >> HandleItems)
--
-- PORTS
-- port items : (Value -> msg) -> Sub msg
