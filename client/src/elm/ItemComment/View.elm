module ItemComment.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, disabled, href, placeholder, required, rows, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.Model exposing (Model, Msg(..), Tab(..))
import User.Model exposing (User)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : Maybe User -> Model -> Html Msg
view muser model =
    let
        mainArea =
            case model.selectedTab of
                Edit ->
                    viewEdit model.comment

                Preview ->
                    viewPreview model.comment
    in
        div []
            [ viewTabs model.selectedTab
            , mainArea
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


viewEdit : String -> Html Msg
viewEdit comment =
    textarea
        [ required True
        , value comment
        , onInput SetComment
        ]
        []


viewPreview : String -> Html Msg
viewPreview comment =
    textarea
        [ required True
        , value comment
        , rows 3
        , disabled True
        ]
        []
