module ItemComment.View
    exposing
        ( view
        , viewItemComments
        )

import App.Types exposing (BackendUrl)
import Backend.Entities exposing (ItemCommentId)
import Backend.Item.Model exposing (ItemComment)
import Backend.Restful exposing (EntityDictList, fromEntityId)
import Editable
import Editable.WebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, cols, disabled, href, id, placeholder, required, rows, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Markdown
import StorageKey exposing (StorageKey)
import User.Model exposing (CurrentUser(..))
import Utils.Html exposing (divider, emptyNode, sectionDivider, showIf, showMaybe)


view : BackendUrl -> CurrentUser -> Html msg
view backendUrl currentUser =
    div [] [ text "ItemComment" ]



--
-- view : BackendUrl -> CurrentUser -> Model -> Html Msg
-- view backendUrl currentUser model =
--     case user of
--         Anonymous ->
--             emptyNode
--
--         Authenticated ( userId, user ) ->
--             let
--                 mainArea =
--                     case model.selectedTab of
--                         Edit ->
--                             viewEdit model.comment
--
--                         Preview ->
--                             viewPreview model.comment
--             in
--                 div []
--                     [ viewTabs model.selectedTab
--                     , form [ class "ui form comment" ]
--                         [ mainArea
--                         , viewActions model
--                         ]
--                     ]
--
--


viewItemComments : CurrentUser -> EntityDictList ItemCommentId (EditableWebData ItemComment) -> Html msg
viewItemComments currentUser comments =
    showIf (not (EveryDictList.isEmpty comments)) <|
        div
            [ class "ui comments" ]
            (comments
                |> EveryDictList.toList
                |> List.map (viewItemComment currentUser)
            )


viewItemComment : CurrentUser -> ( StorageKey ItemCommentId, EditableWebData ItemComment ) -> Html msg
viewItemComment currentUser ( storageKey, editableWebData ) =
    let
        itemCommentId =
            storageKey
                |> StorageKey.value
                |> Maybe.map (fromEntityId >> toString)
                |> Maybe.withDefault ""

        itemComment =
            editableWebData
                |> Editable.WebData.toEditable
                |> Editable.value

        ( authorId, author ) =
            itemComment.user
    in
    div
        [ id <| "comment-" ++ itemCommentId
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
                [ text author.name ]
            , div
                [ class "text" ]
                (Markdown.toHtml Nothing itemComment.comment)
            ]
        ]



--
-- viewTabs : Tab -> Html Msg
-- viewTabs selectedTab =
--     div
--         [ class "ui secondary pointing menu" ]
--         [ a
--             [ classList [ ( "item", True ), ( "active", selectedTab == Edit ) ]
--             , onClick <| SetTab Edit
--             ]
--             [ text "Edit" ]
--         , a
--             [ classList [ ( "item", True ), ( "active", selectedTab == Preview ) ]
--             , onClick <| SetTab Preview
--             ]
--             [ text "Preview" ]
--         ]
--
--
-- viewEdit : String -> Html Msg
-- viewEdit comment =
--     div [ class "field" ]
--         [ textarea
--             [ required True
--             , value comment
--             , onInput SetComment
--             , rows 6
--             , cols 60
--             ]
--             []
--         ]
--
--
-- viewPreview : String -> Html Msg
-- viewPreview comment =
--     div [] <|
--         Markdown.toHtml Nothing comment
--
--
-- viewActions : Model -> Html Msg
-- viewActions model =
--     let
--         isLoading =
--             model.status == Loading
--
--         emptyComment =
--             String.isEmpty model.comment
--
--         attrs =
--             if isLoading || emptyComment then
--                 [ disabled True ]
--             else
--                 [ onClick <| SaveComment
--                 ]
--     in
--         div
--             (attrs
--                 ++ [ classList
--                         [ ( "ui button primary", True )
--                         , ( "loading", isLoading )
--                         , ( "disabled", emptyComment )
--                         ]
--                    ]
--             )
--             [ text "Comment" ]
