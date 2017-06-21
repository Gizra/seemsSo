{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Order where

import Import.NoFoundation
import Model.Types

-- {-| Determine if a given user is allowed to access the PDF file of an item.
-- -}
hasAccessToPdfFileDownload ::
       ( BaseBackend (YesodPersistBackend site) ~ SqlBackend
       , YesodPersist site
       , PersistQueryRead (YesodPersistBackend site)
       )
    => UserId
    -> Text
    -> HandlerT site IO AuthResult
hasAccessToPdfFileDownload userId filename = do
    let unauthorized = Unauthorized "You didn't buy this item"
    mpdf <- runDB $ selectFirst [PdfFileFilename ==. unpack filename] []
    case mpdf of
        Nothing -> return unauthorized
        Just (Entity pdfId _) -> do
            mitem <- runDB $ selectFirst [ItemPdfFile ==. Just pdfId] []
            -- Find the item that references the PDF.
            case mitem of
                Nothing -> return unauthorized
                Just (Entity itemId _) -> do
                    morderItem <-
                        runDB $
                        selectFirst
                            [OrderItemItem ==. itemId, OrderItemUser ==. userId]
                            []
                    -- Find the OrderItem that references the Item, that belongs to the user.
                    case morderItem of
                        Nothing -> return unauthorized
                        Just (Entity _ orderItem) -> do
                            morder <-
                                runDB $
                                selectFirst
                                    [OrderId ==. orderItemOrder orderItem]
                                    []
                            -- Find the Order that the order item belongs to.
                            case morder of
                                Nothing -> return unauthorized
                                Just (Entity _ order)
                                -- Validate it has a "paid" status.
                                 ->
                                    return $
                                    if orderStatus order == OrderStatusPaid
                                        then Authorized
                                        else unauthorized
