{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.OrderSpec
    ( spec
    ) where

import Control.Lens.Fold
import Data.Aeson
import Data.Aeson.Lens
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
                (_, _, _, _, alice, _) <-
                    prepareScenarioWithOrder OrderStatusActive
                bob <- createUser "bob"
                authenticateAs bob
                get RestfulOrderR
                -- Assert empty object, as the order doesn't belong to logged in
                -- user.
                assertEmptyJsonResponse
                statusIs 200
            it "should show the active order to own user" $ do
                (_, _, _, _, alice, _) <-
                    prepareScenarioWithOrder OrderStatusActive
                authenticateAs alice
                get RestfulOrderR
                -- We get a response like: {"data":{"status":"active","id":1}}
                mresponse <- getResponse
                maybe
                    failWithNoResponse
                    (\response ->
                         let statusResult =
                                 simpleBody response ^? key "data" .
                                 key "status" .
                                 _String
                         in assertEq
                                "Order status  is correct"
                                statusResult
                                (Just "active"))
                    mresponse
                statusIs 200
            it "should not show a cancelled order to own user" $ do
                testNoAccessWithOrderStatus OrderStatusCancelled
            it "should not show a payment error order to own user" $ do
                testNoAccessWithOrderStatus OrderStatusPaymentError
            it "should not show a paid order to own user" $ do
                testNoAccessWithOrderStatus OrderStatusPaid

assertEmptyJsonResponse :: YesodExample App ()
assertEmptyJsonResponse = assertJsonResponse (decode "{}" :: Maybe Object)

assertJsonResponse :: Maybe Object -> YesodExample App ()
assertJsonResponse val = do
    mresponse <- getResponse
    maybe
        failWithNoResponse
        (\response ->
             assertEq
                 "Json response is correct"
                 (decode (simpleBody response) :: Maybe Object)
                 val)
        mresponse

{-| Fail an assertion.
-}
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
        runDB $ insert $
        Item "Item1" companyId 10 (Just pdfId) currentTime userId
    return (john, companyId, pdfId, itemId)

prepareScenarioWithOrder ::
       OrderStatus
    -> YesodExample App ( Entity User
                        , CompanyId
                        , PdfFileId
                        , ItemId
                        , Entity User
                        , OrderId)
prepareScenarioWithOrder status = do
    (john, companyId, pdfId, itemId) <- prepareScenario
    alice <- createUser "alice"
    let (Entity userId _) = alice
    authenticateAs alice
    -- Create Order
    currentTime <- liftIO getCurrentTime
    orderId <- runDB $ insert $ Order status userId currentTime
    return (john, companyId, pdfId, itemId, alice, orderId)

testNoAccessWithOrderStatus :: OrderStatus -> YesodExample App ()
testNoAccessWithOrderStatus status = do
    (_, _, _, _, alice, _) <- prepareScenarioWithOrder status
    authenticateAs alice
    get RestfulOrderR
    -- Assert empty object, as the order doesn't belong to logged in
    -- user.
    assertEmptyJsonResponse
    statusIs 200
