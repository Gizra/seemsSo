module ItemComment.Update
    exposing
        ( update
        )

import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Model
import Backend.Restful exposing (EntityDictList)
import EveryDictList exposing (EveryDictList)
import ItemComment.Model exposing (DelegatedMsg(..), Model, Msg(..))
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

        DelegatedSaveComment storageKeys ->
            ( model
              -- Coresponds to`SaveComment` in `Backend.Item.Model`
            , ( partialBackendModel, MsgBackendItem <| Backend.Model.MsgItems <| Backend.Item.Model.SaveComment storageKeys )
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

                                Just comment ->
                                    let
                                        itemUpdated =
                                            { item | comments = EveryDictList.insert commentId comment item.comments }

                                        itemsUpdated =
                                            EveryDictList.insert itemId itemUpdated partialBackendModel.items
                                    in
                                    { partialBackendModel | items = itemsUpdated }
            in
            ( model
            , ( partialBackendModelUpdated, UpdateBackend )
            )
