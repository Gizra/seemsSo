{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulItemComment where

import Import
import Utils.Restful

getRestfulItemCommentR :: ItemId -> ItemCommentId -> Handler Value
getRestfulItemCommentR itemId itemCommentId = do
    itemComment <- runDB $ get404 itemCommentId
    user <- runDB $ get404 (itemCommentUser itemComment)
    urlRender <- getUrlRender
    let itemWithMetaData =
            addEntityMetaData
                urlRender
                (RestfulItemCommentR itemId)
                itemCommentId
                itemComment
    return $ object ["data" .= toJSON itemWithMetaData]
