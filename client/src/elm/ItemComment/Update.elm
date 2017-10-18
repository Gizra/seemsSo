module ItemComment.Update
    exposing
        ( update
        )

import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Item.Utils exposing (getComment, insertComments)
import Backend.Model
import Backend.Restful exposing (EntityDictList)
import Editable
import Editable.WebData
import EveryDictList exposing (EveryDictList)
import ItemComment.Model exposing (DelegatedMsg(..), Model, Msg(..))
import Maybe.Extra exposing (unwrap)
import RemoteData
import StorageKey exposing (StorageKey)


update :
    Msg
    -> Model
    -> ( StorageKey ItemId, { r | items : EntityDictList ItemId Item } )
    -> ( Model, ( { r | items : EntityDictList ItemId Item }, DelegatedMsg ) )
update msg model ( storageKey, partialBackendModel ) =
    case msg of
        SetTab tab ->
            ( { model | selectedTab = tab }
            , ( partialBackendModel, NoOp )
            )

        DelegatedSaveComment ( itemId, commentId ) ->
            let
                itemsUpdated =
                    case EveryDictList.get itemId partialBackendModel.items of
                        Nothing ->
                            partialBackendModel.items

                        Just item ->
                            unwrap partialBackendModel.items
                                (\itemComment ->
                                    let
                                        itemCommentUpdated =
                                            itemComment
                                                |> Editable.WebData.state RemoteData.Loading

                                        itemUpdated =
                                            { item | comments = EveryDictList.insert commentId itemCommentUpdated item.comments }
                                    in
                                    EveryDictList.insert itemId itemUpdated partialBackendModel.items
                                )
                                (getComment ( itemId, commentId ) partialBackendModel.items)
            in
            ( model
              -- Coresponds to`SaveComment` in `Backend.Item.Model`
            , ( { partialBackendModel | items = itemsUpdated }
              , MsgBackendItem <| Backend.Model.MsgItems <| Backend.Item.Model.SaveComment ( itemId, commentId )
              )
            )

        SetComment ( itemId, commentId ) comment ->
            let
                partialBackendModelUpdated =
                    case EveryDictList.get itemId partialBackendModel.items of
                        Nothing ->
                            partialBackendModel

                        Just item ->
                            case EveryDictList.get commentId item.comments of
                                Nothing ->
                                    partialBackendModel

                                Just itemComment ->
                                    let
                                        value =
                                            itemComment
                                                |> Editable.WebData.toEditable
                                                |> Editable.value

                                        valueUpdated =
                                            { value | comment = comment }

                                        itemCommentUpdated =
                                            itemComment
                                                |> Editable.WebData.map (Editable.edit >> Editable.update valueUpdated)

                                        itemUpdated =
                                            { item | comments = EveryDictList.insert commentId itemCommentUpdated item.comments }

                                        itemsUpdated =
                                            EveryDictList.insert itemId itemUpdated partialBackendModel.items
                                    in
                                    { partialBackendModel | items = itemsUpdated }
            in
            ( model
            , ( partialBackendModelUpdated, UpdateBackend )
            )
