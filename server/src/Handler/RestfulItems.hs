{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulItems where

import Import
import Utils.Restful (getEntityList)

getRestfulItemsR :: CompanyId -> Handler Value
getRestfulItemsR companyId = do
    company <- runDB $ get404 companyId
    getEntityList (RestfulItemsR companyId) (RestfulItemR companyId) [ItemCompany ==. companyId]
