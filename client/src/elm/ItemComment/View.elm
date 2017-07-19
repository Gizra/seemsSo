module ItemComment.View
    exposing
        ( view
        , viewItemComments
        )

import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, cols, disabled, href, id, placeholder, required, rows, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.Model exposing (EveryDictListItemComments, ItemComment, ItemCommentId, Model, Msg(..), Tab(..))
import Markdown
import RemoteData exposing (..)
import User.Model exposing (User)
import Utils.Html exposing (divider, emptyNode, sectionDivider, showIf, showMaybe)


view : String -> Maybe User -> Model -> Html Msg
view baseUrl muser model =
    case muser of
        Nothing ->
            emptyNode

        Just user ->
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
                    , form [ class "ui form comment" ]
                        [ mainArea
                        , viewActions model
                        ]
                    ]


viewItemComments : Maybe User -> EveryDictListItemComments -> Html msg
viewItemComments muser commentsDictList =
    showIf (not (EveryDictList.isEmpty commentsDictList)) <|
        div
            [ class "ui comments" ]
            (EveryDictList.toList commentsDictList
                |> List.map (viewItemComment muser)
            )


viewItemComment : Maybe User -> ( ItemCommentId, ItemComment ) -> Html msg
viewItemComment muser ( ItemComment.Model.ItemCommentId itemCommentId, itemComment ) =
    div
        [ id <| "comment-" ++ toString itemCommentId
        , class "comment"
        ]
        [ a
            [ class "avatar" ]
            [ img
                [ src "https://dummyimage.com/80x80/000/fff&text=Avatar" ]
                []
            ]
        , div
            [ class "content" ]
            [ div
                [ class "author" ]
                [ text itemComment.user.name ]
            , div
                [ class "text" ]
                (Markdown.toHtml Nothing itemComment.comment)
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
