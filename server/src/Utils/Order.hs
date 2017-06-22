{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Order where

import Control.Monad.Trans.Maybe
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
    result <-
        runMaybeT $ do
            (Entity pdfId _) <-
                maybeTselectFirst [PdfFileFilename ==. unpack filename]
            -- Find the item that references the PDF.
            (Entity itemId item) <-
                maybeTselectFirst [ItemPdfFile ==. Just pdfId]
            -- Find the company the item belongs to.
            (Entity _ company) <-
                maybeTselectFirst [CompanyId ==. itemCompany item]
            -- Try to return early, if the user is the owner of the item or
            -- company.
            -- @todo: Check if "member" of company.
            if itemUser item == userId || companyUser company == userId
                then return Authorized
              -- Find the OrderItem that references the Item, that belongs to the user.
                else do
                    (Entity _ orderItem) <-
                        maybeTselectFirst
                            [OrderItemItem ==. itemId, OrderItemUser ==. userId]
              -- Find the Order that the order item belongs to.
                    (Entity _ order) <-
                        maybeTselectFirst [OrderId ==. orderItemOrder orderItem]
              -- Validate it has a "paid" status.
                    if orderStatus order == OrderStatusPaid
                        then return Authorized
                        else return unauthorized
    return $ maybe unauthorized id result

maybeTselectFirst ::
       ( PersistEntityBackend record ~ BaseBackend (YesodPersistBackend site)
       , PersistEntity record
       , PersistQueryRead (YesodPersistBackend site)
       , YesodPersist site
       )
    => [Filter record]
    -> MaybeT (HandlerT site IO) (Entity record)
maybeTselectFirst selectOps = MaybeT $ runDB $ selectFirst selectOps []
