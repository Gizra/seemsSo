module ItemComment.Update
    exposing
        ( update
        )

import ItemComment.Model exposing (Model, Msg(..))


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetTab tab ->
            { model | selectedTab = tab }
