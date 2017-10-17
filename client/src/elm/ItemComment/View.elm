module ItemComment.View
    exposing
        ( view
        , viewItemComments
        )

import App.Types exposing (BackendUrl)
import Backend.Entities exposing (ItemCommentId, ItemId)
import Backend.Item.Model exposing (Item, ItemComment)
import Backend.Restful exposing (EntityDictList, fromEntityId)
import Editable
import Editable.WebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, cols, disabled, href, id, placeholder, required, rows, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.Model exposing (Model, Msg(..), Tab(..))
import Markdown
import RemoteData exposing (RemoteData)
import StorageKey exposing (StorageKey)
import User.Model exposing (CurrentUser(..))
import Utils.Html exposing (divider, emptyNode, sectionDivider, showIf, showMaybe)


view : BackendUrl -> CurrentUser -> ( StorageKey ItemId, Item ) -> StorageKey ItemCommentId -> Model -> Html Msg
view backendUrl currentUser ( itemStorageKey, item ) commentStorageKey model =
    case currentUser of
        Anonymous ->
            emptyNode

        Authenticated ( userId, user ) ->
            case EveryDictList.get commentStorageKey item.comments of
                Nothing ->
                    emptyNode

                Just editableWebData ->
                    let
                        mainArea =
                            case model.selectedTab of
                                Edit ->
                                    viewEdit ( itemStorageKey, commentStorageKey ) editableWebData

                                Preview ->
                                    viewPreview editableWebData
                    in
                    div []
                        [ viewTabs model.selectedTab
                        , form [ class "ui form comment" ]
                            [ mainArea
                            , viewActions ( itemStorageKey, commentStorageKey ) editableWebData
                            ]
                        ]


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
    if StorageKey.isNew storageKey then
        emptyNode
    else
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


viewEdit : ( StorageKey ItemId, StorageKey ItemCommentId ) -> EditableWebData ItemComment -> Html Msg
viewEdit storageKeys editableWebData =
    let
        itemComment =
            editableWebData
                |> Editable.WebData.toEditable
                |> Editable.value
    in
    div [ class "field" ]
        [ textarea
            [ required True
            , value <| itemComment.comment
            , onInput <| SetComment storageKeys
            , rows 6
            , cols 60
            ]
            []
        ]


viewPreview : EditableWebData ItemComment -> Html Msg
viewPreview editableWebData =
    let
        itemComment =
            editableWebData
                |> Editable.WebData.toEditable
                |> Editable.value
    in
    if String.isEmpty itemComment.comment then
        div [] [ text "Nothing to preview" ]
    else
        div [] <|
            Markdown.toHtml Nothing itemComment.comment


viewActions : ( StorageKey ItemId, StorageKey ItemCommentId ) -> EditableWebData ItemComment -> Html Msg
viewActions storageKeys editableWebData =
    let
        itemComment =
            editableWebData
                |> Editable.WebData.toEditable
                |> Editable.value

        isLoading =
            editableWebData
                |> Editable.WebData.toWebData
                |> RemoteData.isLoading

        emptyComment =
            String.isEmpty itemComment.comment

        attrs =
            if isLoading || emptyComment then
                [ disabled True ]
            else
                [ onClick <| DelegatedSaveComment storageKeys
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
