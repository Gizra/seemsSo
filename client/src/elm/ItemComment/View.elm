module ItemComment.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.Model exposing (Model, Msg(..), Tab(..))
import User.Model exposing (User)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : Maybe User -> Model -> Html Msg
view muser model =
    div []
        [ viewTabs model.selectedTab
        , text "Item Comment"
        ]


viewTabs : Tab -> Html Msg
viewTabs selectedTab =
    div
        [ class "ui secondary pointing menu" ]
        [ a
            [ classList [ ( "item", True ), ( "active", selectedTab == Edit ) ]
            , onClick <| SetTab Edit
            ]
            [ text "Edit" ]
        , a
            [ classList [ ( "item", True ), ( "active", selectedTab == Preview ) ]
            , onClick <| SetTab Preview
            ]
            [ text "Preview" ]
        ]
