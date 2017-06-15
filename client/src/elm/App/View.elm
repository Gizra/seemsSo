module App.View exposing (..)

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Homepage.View exposing (view)
import Html exposing (..)


view : Model -> Html Msg
view model =
    case model.page of
        Homepage ->
            div []
                [ Html.map MsgPagesHomepage <| Homepage.View.view model.pageHomepage
                ]

        NotFound ->
            div [] [ text "Wrong page defined" ]
