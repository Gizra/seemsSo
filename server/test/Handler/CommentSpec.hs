{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.CommentSpec
    ( spec
    ) where

import TestImport

spec :: Spec
spec =
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
        it "should show comment to anonymous user" $ do
            (john, _, _, itemId) <- prepareScenario
            let (Entity userId _) = john
            let commentText = "comment added by John"
            currentTime <- liftIO getCurrentTime
            _ <-
                runDB $
                insert $ ItemComment commentText itemId userId currentTime
            get $ ItemR itemId
            htmlAnyContain ".ui.comments > .comment > div > div.author" "john"
            htmlAnyContain
                ".ui.comments > .comment > div > div.text"
                (unpack commentText)




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
