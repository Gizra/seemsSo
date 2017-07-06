module App.View exposing (..)

import App.Model exposing (..)
import App.Types exposing (Widget(..))
import Homepage.View exposing (view)
import ItemComment.View exposing (view)
import Html exposing (..)
import Html.Attributes exposing (class)


view : Model -> Html Msg
view model =
    case model.widget of
        HomePage ->
            div [ class "ui container" ]
                [ Html.map MsgPagesHomepage <| Homepage.View.view model.baseUrl model.user model.pageHomepage
                ]

        ItemComment ->
            div [ class "ui container" ]
                [ Html.map MsgPagesItemComment <| ItemComment.View.view model.user model.pageItemComment
                ]

        NotFound ->
            div [] [ text "Wrong page defined" ]
