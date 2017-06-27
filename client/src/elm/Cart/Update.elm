port module Cart.Update
    exposing
        ( subscriptions
        , update
        )

import Cart.Model exposing (Model, Msg(..), emptyModel)
import Dict
import DictList
import Item.Decoder exposing (decodeItemId, decodeItemTuple, decodeItems)
import Json.Decode exposing (Value, decodeValue)
import RemoteData exposing (RemoteData(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem (Ok ( itemId, item )) ->
            let
                items =
                    DictList.insert itemId item model.items

                orderItems =
                    Dict.insert itemId NotAsked model.orderItems
            in
                { model | items = items, orderItems = orderItems } ! []

        AddItem (Err err) ->
            let
                _ =
                    Debug.log "Cart.Update.update // AddItem" err
            in
                model ! []

        ClearCart ->
            emptyModel ! []

        RemoveItem (Ok itemId) ->
            { model | orderItems = Dict.remove itemId model.orderItems } ! []

        RemoveItem (Err err) ->
            let
                _ =
                    Debug.log "Cart.Update.update // RemoveItem" err
            in
                model ! []


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ addItem (decodeValue decodeItemTuple >> AddItem)
        , removeItem (decodeValue decodeItemId >> RemoveItem)
        ]



-- PORTS


port addItem : (Value -> msg) -> Sub msg


port removeItem : (Value -> msg) -> Sub msg
