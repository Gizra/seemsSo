{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.CommentSpec
    ( spec
    ) where

import TestImport

spec :: Spec
spec = do
    withApp $
        describe "Item's comments" $ do
            it "should show 'No comments' to anonymous user" $ do
                (_, _, _, itemId) <- prepareScenario
                get $ ItemR itemId
                htmlAnyContain "section.comments > div" "No comments"
            it "should show 'No comments' to authenticated user" $ do
                (_, _, _, itemId) <- prepareScenario
                alice <- createUser "alice"
                authenticateAs alice
                get $ ItemR itemId
                htmlAnyContain "section.comments > div" "No comments"
            

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
