{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulOrder where

import Import
import Model.Types (OrderStatus(..))
import Utils.Restful

getRestfulOrderR :: Handler Value
getRestfulOrderR = do
    muser <- maybeAuth
    case muser of
        Nothing -> return $ object []
        Just (Entity userId user) -> do
            morder <-
                runDB $
                selectFirst
                    [OrderUser ==. userId, OrderStatus ==. OrderStatusActive]
                    []
            case morder of
                Nothing -> return $ object []
                Just (Entity orderId order) ->
                    return $ object ["data" .= toJSON order]

postRestfulOrderItemR :: Handler Value
postRestfulOrderItemR = error "Handler.RestfulOrder"
