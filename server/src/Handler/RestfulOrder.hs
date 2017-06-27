{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.RestfulOrder where

import Database.Persist.Sql (fromSqlKey)
import Import
import Model.Types (OrderStatus(..))
import Utils.Form
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

getOrderR :: OrderId -> Handler Html
getOrderR orderId = do
    order <- runDB $ get404 orderId
    let orderStatusLabel = getOrderStatusLabel $ orderStatus order
    defaultLayout $ do
        setTitle . toHtml $ "Order #" ++ (show $ fromSqlKey orderId)
        $(widgetFile "order")

getOrderStatusLabel :: OrderStatus -> Text
getOrderStatusLabel orderStatus = pack $ drop 11 $ show orderStatus

orderForm :: UserId -> Maybe Order -> Form Order
orderForm userId morder =
    renderSematnicUiDivs $
    Order <$>
    areq
        (selectField statusOptions)
        (selectSettings "Status")
        (orderStatus <$> morder) <*>
    pure userId <*>
    lift (liftIO getCurrentTime)
  where
    statusOptions =
        optionsPairs $
        map (\x -> (getOrderStatusLabel x, x)) [minBound .. maxBound]

getCreateOrderR :: Handler Html
getCreateOrderR = do
    (userId, _) <- requireAuthPair
    (widget, enctype) <- generateFormPost $ orderForm userId Nothing
    defaultLayout $(widgetFile "order-create")

postCreateOrderR :: Handler Html
postCreateOrderR = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ orderForm userId Nothing
    case result of
        FormSuccess order -> do
            orderId <- runDB $ insert order
            setMessage "Order saved"
            redirect $ OrderR orderId
        _ ->
            defaultLayout
                [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{CreateOrderR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

getEditOrderR :: OrderId -> Handler Html
getEditOrderR orderId = do
    order <- runDB $ get404 orderId
    (userId, _) <- requireAuthPair
    (widget, enctype) <- generateFormPost $ orderForm userId (Just order)
    defaultLayout $(widgetFile "order-update")

postEditOrderR :: OrderId -> Handler Html
postEditOrderR orderId = do
    (userId, _) <- requireAuthPair
    ((result, widget), enctype) <- runFormPost $ orderForm userId Nothing
    case result of
        FormSuccess order -> do
            _ <- updateOrder orderId order
            setMessage "Order updated"
            redirect $ OrderR orderId
        _ -> do
            setMessage "Saving failed"
            defaultLayout $(widgetFile "order-update")

updateOrder :: Key Order -> Order -> Handler Bool
updateOrder orderId order = do
    let validations = []
    let lefts' = lefts validations
    if not $ null lefts'
        then return False
        else do
            _ <- runDB $ replace orderId order
            return True
