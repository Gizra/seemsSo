module Pages.Item.Update
    exposing
        ( update
        )

import App.Types exposing (BackendUrl)
import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Model
import Backend.Restful exposing (EntityDictList)
import Backend.Update
import ItemComment.Model
import ItemComment.Update
import Pages.Item.Model exposing (DelegatedMsg(..), Model, Msg(..))
import StorageKey exposing (StorageKey)


update :
    BackendUrl
    -> Msg
    -> Model
    -> ( StorageKey ItemId, { r | items : EntityDictList ItemId Item } )
    -> ( Model, Cmd Msg, ( { r | items : EntityDictList ItemId Item }, DelegatedMsg ) )
update backendUrl msg model ( storageKey, partialBackendModel ) =
    case msg of
        MsgItemComment subMsg ->
            let
                ( subModel, ( partialBackendModelUpdated, delegatedMsg ) ) =
                    ItemComment.Update.update subMsg model.itemComment ( storageKey, partialBackendModel )

                modelUpdated =
                    { model | itemComment = subModel }

                delegatedMsgs =
                    case delegatedMsg of
                        ItemComment.Model.NoOp ->
                            NoOp

                        ItemComment.Model.MsgBackendItem backendItemMsg ->
                            MsgBackendItem backendItemMsg
            in
            ( modelUpdated
            , Cmd.none
            , ( partialBackendModelUpdated, delegatedMsgs )
            )

        SetComment storageKey comment ->
            ( model
            , Cmd.none
            , ( partialBackendModel, NoOp )
            )
