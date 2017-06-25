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
    -- We don't want to return a 403, so we just return an empty object.
    case muser of
        Nothing -> return $ object []
        Just (Entity userId user) -> do
            morder <-
                runDB $
                selectFirst
                    [OrderUser ==. userId, OrderStatus ==. OrderStatusActive]
                    []
            -- We don't want to return a 403, so we just return an empty object.
            case morder of
                Nothing -> return $ object []
                Just order -> return $ object ["data" .= toJSON order]

postRestfulOrderItemR :: ItemId -> Handler Value
postRestfulOrderItemR itemId = do
    (Entity userId user) <- requireAuth
    morder <-
        runDB $
        selectFirst [OrderUser ==. userId, OrderStatus ==. OrderStatusActive] []
    -- Determine if an active order exists.
    orderId <-
        case morder of
            Nothing -> do
                currentTime <- liftIO getCurrentTime
                runDB $ insert $ Order OrderStatusActive userId currentTime
            Just (Entity orderId _) -> return orderId
    existingOrderItem <-
        runDB $
        selectFirst [OrderItemOrder ==. orderId, OrderItemItem ==. itemId] []
    case existingOrderItem of
        Just _ -> invalidArgs ["Order item with Item ID already exists"]
        Nothing -> do
            _ <- runDB $ insert $ OrderItem orderId itemId userId
            -- Return the whole Order JSON.
            getRestfulOrderR
