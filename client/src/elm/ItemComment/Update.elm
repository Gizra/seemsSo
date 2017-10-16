module ItemComment.Update
    exposing
        ( update
        )

import ItemComment.Model exposing (DelegatedMsg(..), Model, Msg(..))


update : Msg -> Model -> ( Model, DelegatedMsg )
update msg model =
    case msg of
        SetTab tab ->
            ( { model | selectedTab = tab }
            , NoOp
            )

        DelegatedSaveComment values ->
            ( model, SaveComment values )
