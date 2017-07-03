port module ItemComment.Update
    exposing
        ( update
        )

import ItemComment.Model exposing (Model, Msg(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []
