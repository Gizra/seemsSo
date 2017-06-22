{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.OrderSpec
    ( spec
    ) where

import Handler.PdfFile
import Model.Types (OrderStatus(..))
import System.Directory (copyFile)
import TestImport
import Yesod.Static

spec :: Spec
spec = do
    withApp $
        describe "PDF file download" $ do
            it "should redirect to login page anonymous user" $ do
                _ <- prepareScenario
                get pdfFileStaticRoute
                statusIs 303
            it
                "should not allow access to authenticated user that didn't buy the item" $ do
                _ <- prepareScenario
                alice <- createUser "alice"
                authenticateAs alice
                get pdfFileStaticRoute
                statusIs 403
            it "should allow access to admin user that didn't buy the item" $ do
                _ <- prepareScenario
                adminUser <- createUser "adminUser"
                let (Entity adminUid _) = adminUser
                -- Create "admin" role, and assign it to the user.
                roleId <- runDB . insert $ Role "admin"
                _ <- runDB . insert $ UserRole roleId adminUid
                authenticateAs adminUser
                get pdfFileStaticRoute
                statusIs 200
            it "should allow access to the owner, that didn't buy the item" $ do
                (ownerUser, _, _, _) <- prepareScenario
                authenticateAs ownerUser
                get pdfFileStaticRoute
                statusIs 200
            it "should not allow access to user with 'active' order" $
                testWithOrderStatus OrderStatusActive 403
            it "should not allow access to user with 'cancelled' order" $
                testWithOrderStatus OrderStatusCancelled 403
            it "should not allow access to user with 'payment error' order" $
                testWithOrderStatus OrderStatusPaymentError 403
            it "should allow access to authenticated user that bought the item" $
                testWithOrderStatus OrderStatusPaid 200

pdfFileStaticRoute :: Route App
pdfFileStaticRoute = StaticR $ StaticRoute ["item-pdf", "item1.pdf"] []

testWithOrderStatus :: OrderStatus -> Int -> YesodExample App ()
testWithOrderStatus orderStatus httpStatus = do
    (_, _, _, itemId) <- prepareScenario
    alice <- createUser "alice"
    authenticateAs alice
    -- Create a paid order.
    prepareOrder alice itemId orderStatus
    get pdfFileStaticRoute
    statusIs httpStatus

prepareOrder ::
       Entity User -> ItemId -> OrderStatus -> YesodExample App (Key OrderItem)
prepareOrder user itemId orderStatus = do
    let (Entity userId _) = user
    -- Create a paid order.
    currentTime <- liftIO getCurrentTime
    orderId <- runDB $ insert $ Order orderStatus userId currentTime
    runDB $ insert $ OrderItem orderId itemId userId

prepareScenario :: YesodExample App (Entity User, CompanyId, PdfFileId, ItemId)
prepareScenario = do
    let filename = "item1.pdf"
    _ <- liftIO $ copyFile ("migrate-files/" ++ filename) (pdfFilePath filename)
    currentTime <- liftIO getCurrentTime
    john <- createUser "john"
    let (Entity userId user) = john
    companyId <- runDB $ insert $ Company "company1" currentTime userId
    pdfId <- runDB $ insert $ PdfFile filename currentTime
    itemId <-
        runDB $
        insert $ Item "Item1" companyId 10 (Just pdfId) currentTime userId
    return (john, companyId, pdfId, itemId)
