{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Item where

import Database.Persist.Sql (fromSqlKey)
import Import
import Utils.Form (renderSematnicUiDivs)

getItemR :: ItemId -> Handler Html
getItemR itemId = do
    item <- runDB $ get404 itemId
    company <- runDB $ get404 $ itemCompany item
    defaultLayout $ do
        setTitle . toHtml $ "Item #" ++ (show $ fromSqlKey itemId)
        $(widgetFile "item")

getCreateItemR :: Handler Html
getCreateItemR = do
    (userId, _) <- requireAuthPair
    (widget, enctype) <- generateFormPost $ itemForm userId Nothing
    defaultLayout $(widgetFile "item-create")

postCreateItemR :: Handler Html
postCreateItemR = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ itemForm userId Nothing
    case result of
        FormSuccess item -> do
            itemId <- runDB $ insert item
            setMessage "Item saved"
            redirect $ ItemR itemId
        _ ->
            defaultLayout
                [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{CreateItemR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

getEditItemR :: ItemId -> Handler Html
getEditItemR itemId = do
    item <- runDB $ get404 itemId
    (userId, _) <- requireAuthPair
    (widget, enctype) <- generateFormPost $ itemForm userId (Just item)
    defaultLayout $(widgetFile "item-update")

postEditItemR :: ItemId -> Handler Html
postEditItemR itemId = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ itemForm userId Nothing
    case result of
        FormSuccess item -> do
            _ <- updateItem itemId item
            setMessage "Item updated"
            redirect $ ItemR itemId
        _ -> do
            setMessage "Saving failed."
            defaultLayout $(widgetFile "item-update")

updateItem :: Key Item -> Item -> Handler Bool
updateItem itemId item = do
    let validations = [validateItemPrice $ itemPrice item]
    let lefts' = lefts validations
    if not $ null lefts'
        then return False
        else do
            _ <- runDB $ replace itemId item
            return True

validateItemPrice :: Int -> Either Text Int
validateItemPrice price =
    if price <= 0
        then Left "Price should be above 0"
        else Right price

itemForm :: UserId -> Maybe Item -> Form Item
itemForm userId mitem =
    renderSematnicUiDivs $
    Item <$> areq textField "Name" (itemName <$> mitem) <*>
    areq
        (selectField companies)
        (selectSettings "Company")
        (itemCompany <$> mitem) <*>
    areq priceField "Price" (itemPrice <$> mitem) <*>
    lift (liftIO getCurrentTime) <*>
    pure userId
  where
    selectSettings label =
        FieldSettings
        { fsLabel = label
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [("class", "ui fluid dropdown")]
        }
    -- @todo: Generalize.
    companies = do
        entities <- runDB $ selectList [] [Asc CompanyName]
        optionsPairs $
            map
                (\entity -> (companyName $ entityVal entity, entityKey entity))
                entities
    priceField = check validateItemPrice intField
