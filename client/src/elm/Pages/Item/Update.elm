module Pages.Item.Update
    exposing
        ( update
        )

import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Restful exposing (EntityDictList)
import ItemComment.Update
import Pages.Item.Model exposing (Model, Msg(..))


update : Msg -> Model -> EntityDictList ItemId Item -> ItemId -> ( Model, Cmd Msg )
update msg model items currentItemId =
    case msg of
        MsgItemComment subMsg ->
            ( { model | itemComment = ItemComment.Update.update subMsg model.itemComment }
            , Cmd.none
            )

        SetComment storageKey comment ->
            ( model, Cmd.none )
