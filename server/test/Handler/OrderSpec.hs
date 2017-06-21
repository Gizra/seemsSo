{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.OrderSpec
    ( spec
    ) where

import Model.Types (OrderStatus(..))
import System.Directory (copyFile)
import TestImport
import Yesod.Static

spec :: Spec
spec = do
    let pdfFileStaticRoute = StaticR $ StaticRoute ["item-pdf", "item1.pdf"] []
    withApp $
        describe "PDF file download" $ do
            it "should not allow access to anonymous user" $ do
                (_, _, _, itemId) <- prepareScenario
                get pdfFileStaticRoute
                statusIs 403
            it
                "should not allow access to authenticated user that didn't buy the item" $ do
                (_, _, _, itemId) <- prepareScenario
                alice <- createUser "alice"
                authenticateAs alice
                get pdfFileStaticRoute
                statusIs 403
            it
                "should not allow access to authenticated user that bought the item" $ do
                (_, _, _, itemId) <- prepareScenario
                alice <- createUser "alice"
                let (Entity userId _) = alice
                authenticateAs alice
                -- Create a paid order.
                currentTime <- liftIO getCurrentTime
                orderId <-
                    runDB $ insert $ Order OrderStatusPaid userId currentTime
                orderItemId <- runDB $ insert $ OrderItem orderId itemId userId
                get pdfFileStaticRoute
                statusIs 200

prepareScenario :: YesodExample App (Entity User, CompanyId, PdfFileId, ItemId)
prepareScenario = do
    let filename = "item1.pdf"
    -- _ <- copyFile ("migrate-files/" ++ filename) (pdfFilePath filename)
    currentTime <- liftIO getCurrentTime
    john <- createUser "john"
    let (Entity userId user) = john
    companyId <- runDB $ insert $ Company "company1" currentTime userId
    pdfId <- runDB $ insert $ PdfFile filename currentTime
    itemId <-
        runDB $
        insert $ Item "Item1" companyId 10 (Just pdfId) currentTime userId
    return (john, companyId, pdfId, itemId)
