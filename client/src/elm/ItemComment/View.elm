module ItemComment.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, cols, disabled, href, placeholder, required, rows, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.Model exposing (Model, Msg(..), Tab(..))
import Markdown
import RemoteData exposing (..)
import User.Model exposing (User)
import Utils.Html exposing (divider, sectionDivider, showIf, showMaybe)


view : String -> Maybe User -> Model -> Html Msg
view baseUrl muser model =
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
            , form [ class "ui form" ]
                [ mainArea
                , viewActions model
                ]
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
    div [ class "field" ]
        [ textarea
            [ required True
            , value comment
            , onInput SetComment
            , rows 6
            , cols 60
            ]
            []
        ]


viewPreview : String -> Html Msg
viewPreview comment =
    div [] <|
        Markdown.toHtml Nothing comment


viewActions : Model -> Html Msg
viewActions model =
    let
        isLoading =
            model.status == Loading

        emptyComment =
            String.isEmpty model.comment

        attrs =
            if isLoading || emptyComment then
                [ disabled True ]
            else
                [ onClick <| SaveComment
                ]
    in
        div
            (attrs
                ++ [ classList
                        [ ( "ui button primary", True )
                        , ( "loading", isLoading )
                        , ( "disabled", emptyComment )
                        ]
                   ]
            )
            [ text "Comment" ]
