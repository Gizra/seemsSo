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
import ItemComment.Model exposing (DelegatedMsg(..))
import ItemComment.Update
import Pages.Item.Model exposing (Model, Msg(..))


update : BackendUrl -> Msg -> Model -> Backend.Model.Model -> EntityDictList ItemId Item -> ItemId -> ( Model, Cmd Msg, ( Backend.Model.Model, Cmd Backend.Model.Msg ) )
update backendUrl msg model backendModel items currentItemId =
    let
        noBackendChange =
            ( backendModel, Cmd.none )
    in
    case msg of
        MsgBackendItem subMsg ->
            let
                ( subModel, backendMsg ) =
                    Backend.Update.update backendUrl subMsg backendModel
            in
            ( model
            , Cmd.none
            , ( subModel, backendMsg )
            )

        MsgItemComment subMsg ->
            let
                ( subModel, delegatedMsg ) =
                    ItemComment.Update.update subMsg model.itemComment

                modelUpdated =
                    { model | itemComment = subModel }

                backendChanges =
                    case delegatedMsg of
                        NoOp ->
                            noBackendChange

                        SaveComment values ->
                            let
                                ( _, _, changes ) =
                                    update backendUrl (MsgBackendItem <| Backend.Model.MsgItems <| Backend.Item.Model.SaveComment values) modelUpdated backendModel items currentItemId
                            in
                            changes
            in
            ( modelUpdated
            , Cmd.none
            , backendChanges
            )

        SetComment storageKey comment ->
            ( model
            , Cmd.none
            , noBackendChange
            )
