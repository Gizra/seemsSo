module App.View exposing (..)

import App.Model exposing (..)
import App.Types exposing (Widget(..))
import Homepage.View exposing (view)
import Html exposing (..)


view : Model -> Html Msg
view model =
    case model.widget of
        HomePage ->
            div []
                [ Html.map MsgPagesHomepage <| Homepage.View.view model.pageHomepage
                ]

        NotFound ->
            div [] [ text "Wrong page defined" ]
