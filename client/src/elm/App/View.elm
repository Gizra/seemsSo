module App.View exposing (..)

import App.Model exposing (..)
import App.Types exposing (Page(..))
import Pages.Homepage.View exposing (view)
import Pages.Item.View exposing (view)
import Html exposing (..)
import Html.Attributes exposing (class)


view : Model -> Html Msg
view model =
    case model.activePage of
        -- HomePage ->
        --     div [ class "ui container" ]
        --         [ Html.map MsgPagesHomepage <| Pages.Homepage.View.view model.baseUrl model.user model.pageHomepage
        --         ]
        Item itemId ->
            div [ class "ui container" ]
                [ Pages.Item.View.view model.baseUrl model.user

                -- Html.map MsgPagesItem <| Pages.Item.View.view model.baseUrl model.user model.pageItem
                ]

        NotFound ->
            div [] [ text "Wrong page defined" ]
