{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulItem where

import Import
import Model.Types (SaleStatus(..))
import Utils.Restful

getRestfulItemR :: CompanyId -> ItemId -> Handler Value
getRestfulItemR companyId itemId = do
    sale <- runDB $ get404 companyId
    if (saleStatus sale == SaleStatusActive)
            -- Active sale.
        then do
            item <- runDB $ get404 itemId
            if (itemSale item == CompanyId)
                then do
                    urlRender <- getUrlRender
                    let itemWithMetaData =
                            addEntityMetaData
                                urlRender
                                (RestfulItemR CompanyId)
                                itemId
                                item
                    return $ object ["data" .= toJSON itemWithMetaData]
                else invalidArgs
                         [ "Item's sale doesn't match the Sale ID you have passed."
                         ]
        else invalidArgs
                 ["Cannot get items for a Sale that is not currently active."]
