{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ItemSpec
    ( spec
    ) where

import TestImport

spec :: Spec
spec = do
    withApp $
        describe "Item page" $ do
            it "asserts access as anonymous user" $ do
                (_, _, _, itemId) <- prepareScenario
                get $ ItemR itemId
                statusIs 200
            it "asserts access as authenticated user" $ do
                (_, _, _, itemId) <- prepareScenario
                alice <- createUser "alice"
                authenticateAs alice
                get $ ItemR itemId
                statusIs 200
            it "asserts access as item owner" $ do
                (userId, _, _, itemId) <- prepareScenario
                authenticateAs userId
                get $ ItemR itemId
                statusIs 200
            -- @todo: Currenly for any auth user.
            it "should not show the download link for privileged users" $ do
                (_, _, _, itemId) <- prepareScenario
                get $ ItemR itemId
                htmlCount ".ui.segment > .download" 0
            it "should show the download link for privileged users" $ do
                (userId, _, _, itemId) <- prepareScenario
                authenticateAs userId
                get $ ItemR itemId
                htmlCount ".ui.segment > .download" 1

prepareScenario :: YesodExample App (Entity User, CompanyId, PdfFileId, ItemId)
prepareScenario = do
    currentTime <- liftIO getCurrentTime
    john <- createUser "john"
    let (Entity userId user) = john
    companyId <- runDB $ insert $ Company "company1" currentTime userId
    pdfId <- runDB $ insert $ PdfFile "someFileName" currentTime
    itemId <-
        runDB $
        insert $ Item "Item1" companyId 10 (Just pdfId) currentTime userId
    return (john, companyId, pdfId, itemId)
