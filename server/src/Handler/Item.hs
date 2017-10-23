{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Item where

import Data.Aeson.Text (encodeToLazyText)
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Database.Persist.Sql (fromSqlKey)
import Handler.PdfFile (pdfFilePath, writeToServer)
import Import
import Text.Julius (rawJS)
import Utils.Elm (ElmWidgetFlags(..))
import Utils.Form (renderSematnicUiDivs)
import Utils.Item (ItemMeta(..))
import Utils.ItemComment (getEncodedItemCommentsByItemId)

getItemR :: ItemId -> Handler Html
getItemR itemId = do
    item <- runDB $ get404 itemId
    let itemJson = encodeToLazyText $ Entity itemId item
    company <- runDB $ get404 $ itemCompany item
    let itemMeta =
            ItemMeta
            { itemMetaItem = Entity itemId item
            , itemMetaCompany = Entity (itemCompany item) company
            }
    let itemMetaJson = encodeToLazyText $ toJSON itemMeta
    comments <- getEncodedItemCommentsByItemId itemId
    mpdf <-
        maybe
            (return Nothing)
            (\pdfId -> do
                 routeAccess <- isAuthorized (PdfFileR pdfId) False
                 case routeAccess of
                     Authorized -> runDB $ selectFirst [PdfFileId ==. pdfId] []
                     _ -> return Nothing)
            (itemPdfFile item)
    muser <- maybeAuthPair
    let userJson = encodeToLazyText $ maybe Null (toJSON . uncurry Entity) muser
    let elmFlags =
            encodeToLazyText $
            toJSON
                ElmWidgetFlags
                { elmWidgetFlagsPage = "item" :: Text
                , elmWidgetFlagsEntityId =
                      Just $ fromIntegral (fromSqlKey itemId)
                }
    let elmAppWidget = $(widgetFile "elm") :: Widget
    defaultLayout $ do
        setTitle . toHtml $ "Item #" ++ show (fromSqlKey itemId)
        addScript $ StaticR js_Main_js
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
        FormSuccess (item, (file, date))
            -- Save the PDF file
            -- @todo: Make helper function
         -> do
            filename <- writeToServer file
            pdfId <- runDB $ insert $ PdfFile filename date
            itemId <- runDB $ insert (item {itemPdfFile = Just pdfId})
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
        FormSuccess (item, (file, date)) -> do
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

itemForm :: UserId -> Maybe Item -> Form (Item, (FileInfo, UTCTime))
itemForm userId mitem =
    renderSematnicUiDivs $
    (,) <$>
    (Item <$> areq textField "Name" (itemName <$> mitem) <*>
     areq
         (selectField companies)
         (selectSettings "Company")
         (itemCompany <$> mitem) <*>
     areq priceField "Price" (itemPrice <$> mitem) <*>
     pure (mitem >>= itemPdfFile) <*>
     lift (liftIO getCurrentTime) <*>
     pure userId) <*>
    ((,) <$> fileAFormReq "PDF file" <*> lift (liftIO getCurrentTime))
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
