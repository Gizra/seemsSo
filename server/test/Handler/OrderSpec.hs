{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.OrderSpec
    ( spec
    ) where

import Data.Aeson
import Handler.PdfFile
import Model.Types (OrderStatus(..))
import Network.Wai.Test (SResponse(..))
import System.Directory (copyFile)
import TestImport
import Yesod.Static

spec :: Spec
spec = do
    withApp $ do
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
        describe "Order RESTful" $ do
            it "should allow access to anonymous user" $ do
                get RestfulOrderR
                -- Assert empty object
                assertEmptyJsonResponse
                statusIs 200
            it "should allow access to authenticated user" $ do
                get RestfulOrderR
                -- Assert empty object
                assertEmptyJsonResponse
                statusIs 200

assertEmptyJsonResponse :: YesodExample App ()
assertEmptyJsonResponse = assertJsonResponse (Nothing :: Maybe Int)

assertJsonResponse :: Maybe Int -> YesodExample App ()
assertJsonResponse val = do
    mresponse <- getResponse
    maybe
        failWithNoResponse
        (\response ->
             assertEq "same" (decode (simpleBody response) :: Maybe Int) val)
        mresponse

failWithNoResponse :: YesodExample App ()
failWithNoResponse = assertEq "Response is missing" (0 :: Int) (1 :: Int)

pdfFileStaticRoute :: Route App
pdfFileStaticRoute = StaticR $ StaticRoute ["item-pdf", "item1.pdf"] []

testWithOrderStatus :: OrderStatus -> Int -> YesodExample App ()
testWithOrderStatus orderStatus httpStatus = do
    (_, _, _, itemId) <- prepareScenario
    alice <- createUser "alice"
    authenticateAs alice
    -- Create a paid order.
    _ <- prepareOrder alice itemId orderStatus
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
