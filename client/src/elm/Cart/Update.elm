port module Cart.Update
    exposing
        ( subscriptions
        , update
        )

import Cart.Model exposing (Model, Msg(..), emptyModel)
import Item.Decoder exposing (decodeItemId, decodeItems)
import Json.Decode exposing (Value, decodeValue)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem (Ok itemId) ->
            { model | items = itemId :: model.items } ! []

        AddItem (Err err) ->
            let
                _ =
                    Debug.log "Cart.Update.update // AddItem" err
            in
                model ! []

        ClearCart ->
            emptyModel ! []

        RemoveItem (Ok itemId) ->
            let
                itemsUpdated =
                    List.filter (\val -> val /= itemId) model.items
            in
                { model | items = itemsUpdated } ! []

        RemoveItem (Err err) ->
            let
                _ =
                    Debug.log "Cart.Update.update // RemoveItem" err
            in
                model ! []


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ addItem (decodeValue decodeItemId >> AddItem)
        , removeItem (decodeValue decodeItemId >> RemoveItem)
        ]



-- PORTS


port addItem : (Value -> msg) -> Sub msg


port removeItem : (Value -> msg) -> Sub msg
