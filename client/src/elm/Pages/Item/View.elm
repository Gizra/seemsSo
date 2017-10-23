module Pages.Item.View exposing (..)

import Amount exposing (showAmountWithCurrency)
import App.Types exposing (BackendUrl(..))
import Backend.Entities exposing (ItemId)
import Backend.Item.Model exposing (Item)
import Backend.Restful exposing (EntityDictList, fromEntityId)
import Currency.Model exposing (Currency(USD))
import EveryDictList exposing (EveryDictList)
import Html exposing (..)
import Html.Attributes exposing (alt, class, classList, href, placeholder, src, style, target, type_, value)
import Html.Events exposing (onClick, onInput)
import ItemComment.View exposing (viewItemComments)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Item.Model exposing (Model, Msg)
import StorageKey exposing (StorageKey(Existing))
import User.Model exposing (CurrentUser(..))
import Utils.Html exposing (divider, emptyNode, sectionDivider, showIf, showMaybe)


view : BackendUrl -> CurrentUser -> EntityDictList ItemId Item -> StorageKey ItemId -> Model -> Html Msg
view backendUrl currentUser items itemStorageKey model =
    unwrap emptyNode
        (\item ->
            div []
                [ h1 [] [ text item.name ]
                , viewCompany backendUrl item
                , viewPrice item
                , viewItemComments currentUser item.comments
                , Html.map Pages.Item.Model.MsgItemComment <| ItemComment.View.view backendUrl currentUser ( itemStorageKey, item ) StorageKey.New model.itemComment
                ]
        )
        (EveryDictList.get itemStorageKey items)


viewPrice : Item -> Html Msg
viewPrice item =
    div
        [ class "ui huge labels" ]
        [ div
            [ class "ui label" ]
            [ showAmountWithCurrency item.price USD ]
        ]


viewCompany : BackendUrl -> Item -> Html Msg
viewCompany (BackendUrl backendUrl) item =
    unwrap emptyNode
        (\company ->
            let
                companyId =
                    company.id
                        |> StorageKey.value
                        |> Maybe.map (fromEntityId >> toString)
                        |> Maybe.withDefault ""
            in
            div []
                [ -- @todo: Make href type safe.
                  a [ href <| backendUrl ++ "/company/" ++ companyId ] [ text company.name ]
                ]
        )
        item.company
