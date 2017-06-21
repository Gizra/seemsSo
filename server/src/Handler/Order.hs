{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Order where

import Import
import Model.Types

{-| Determine if a given user is allowed to access the PDF file of an item.
-}
hasAccessToPdfFileDownload :: UserId -> String -> Handler Bool
hasAccessToPdfFileDownload userId filename = do
    (Entity pdfId _) <- runDB $ selectFirst [PdfFileFilename ==. filename] []
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
    return $ orderStatus order == OrderStatusPaid
