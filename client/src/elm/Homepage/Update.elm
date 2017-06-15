port module Contact.Update
    exposing
        ( subscriptions
        , update
        )

import Contact.Decoder exposing (decodeContacts)
import Contact.Model exposing (Model, Msg(..))
import Json.Decode exposing (Value, decodeValue)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleItems (Ok values) ->
            { model | items = values } ! []

        HandleItems (Err err) ->
            let
                _ =
                    Debug.log "HandleContacts" err
            in
                model ! []


subscriptions : Sub Msg
subscriptions =
    items (decodeValue decodeItems >> HandleItems)



-- PORTS


port items : (Value -> msg) -> Sub msg
