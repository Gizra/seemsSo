{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulItem where

import Import
import Utils.Restful

getRestfulItemR :: CompanyId -> ItemId -> Handler Value
getRestfulItemR companyId itemId = do
    item <- runDB $ get404 itemId
    urlRender <- getUrlRender
    let itemWithMetaData = addEntityMetaData urlRender (RestfulItemR companyId) itemId item
    return $ object ["data" .= toJSON itemWithMetaData]
