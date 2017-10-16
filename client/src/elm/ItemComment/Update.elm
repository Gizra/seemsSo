module ItemComment.Update
    exposing
        ( update
        )

import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Model
import Backend.Restful exposing (EntityDictList)
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

        DelegatedSaveComment values ->
            ( model
            , ( partialBackendModel, MsgBackendItem <| Backend.Model.MsgItems <| Backend.Item.Model.SaveComment values )
            )
