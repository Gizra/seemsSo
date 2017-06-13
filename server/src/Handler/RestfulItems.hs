{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulItems where

import Import
import Utils.Restful (getEntityList)

getRestfulItemsR :: CompanyId -> Handler Value
getRestfulItemsR CompanyId = do
    company <- runDB $ get404 CompanyId
    case saleStatus sale of
        SaleStatusActive ->
            getEntityList
                (RestfulItemsR CompanyId)
                (RestfulItemR CompanyId)
                selectFilters
            where selectFilters = [ItemSale ==. CompanyId]
        _
                  -- Don't show items for non-active sales.
         ->
            invalidArgs
                ["Cannot get items for a Sale that is not currently active."]
