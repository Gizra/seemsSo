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
    (Entity pdfId _) <- runDB $ selectFirst [PdfFileFilename ==. unpack filename] []
    -- Find the item that references the PDF.
    (Entity itemId _) <- runDB $ selectFirst [ItemPdfFile ==. pdfId] []
    -- Find the OrderItem that references the Item, that belongs to the user
    (Entity _ orderItem) <-
        runDB $
        selectFirst [OrderItemItem ==. itemId, OrderItemUser ==. userId] []
    -- Find the Order that the order item belongs to.
    (Entity _ order) <-
        runDB $ selectFirst [OrderId ==. orderItemOrder orderItem] []
    -- Validate it has a "paid" status.
    return $
        if orderStatus order == OrderStatusPaid
            then Authorized
            else unauthorized
