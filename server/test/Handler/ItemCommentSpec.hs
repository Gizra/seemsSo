{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.ItemCommentSpec
    ( spec
    ) where

import Data.Aeson
import TestImport

spec :: Spec
spec =
    withApp $ do
        describe "Item's comments" $ do
            it "should show 'No comments' to anonymous user" $ do
                (_, _, _, itemId) <- prepareScenarioWithoutComment
                get $ ItemR itemId
                htmlAnyContain "section.comments > div" "No comments"
            it "should show 'No comments' to authenticated user" $ do
                (_, _, _, itemId) <- prepareScenarioWithoutComment
                bob <- createUser "bob"
                authenticateAs bob
                get $ ItemR itemId
                htmlAnyContain "section.comments > div" "No comments"
            it "should show comment to anonymous user" $ do
                (_, _, _, itemId, _, _) <- prepareScenario
                assertCommentExists itemId
            it "should show comment to autheenticated user" $ do
                (_, _, _, itemId, _, _) <- prepareScenario
                bob <- createUser "bob"
                authenticateAs bob
                assertCommentExists itemId
            it "should show comment to comment owner" $ do
                (_, _, _, itemId, alice, _) <- prepareScenario
                authenticateAs alice
                assertCommentExists itemId
        describe "Item's comments via RESTful" $ do
            it
                "should show allow access to anonymous user when there are no comments" $ do
                (_, _, _, itemId) <- prepareScenarioWithoutComment
                get $ RestfulItemCommentsR itemId
                statusIs 200
            it
                "should show allow access to anonymous user when there are comments" $ do
                (_, _, _, itemId, _, _) <- prepareScenario
                get $ RestfulItemCommentsR itemId
                statusIs 200
            it
                "should show allow access to authenticated user when there are no comments" $ do
                (_, _, _, itemId) <- prepareScenarioWithoutComment
                bob <- createUser "bob"
                authenticateAs bob
                get $ RestfulItemCommentsR itemId
                statusIs 200
            it
                "should show allow access to authenticated user when there are comments" $ do
                (_, _, _, itemId, _, _) <- prepareScenario
                bob <- createUser "bob"
                authenticateAs bob
                get $ RestfulItemCommentsR itemId
                statusIs 200
            it "should not allow anonymous user to create a comment" $ do
                assertPostComment False
            it "should allow authenticated user to create a comment" $ do
                createUserWithAccessToken "bob"
                assertPostComment True

{-| Go to Item's page, and assert the comment exist.
-}
assertCommentExists :: ItemId -> YesodExample App ()
assertCommentExists itemId = do
    get $ ItemR itemId
  -- Assert avatar.
    htmlCount ".ui.comments > .comment > .avatar > img" 1
  -- Assert comment author name.
    htmlAnyContain ".ui.comments > .comment > div > div.author" "alice"
  -- Assert comment text.
    htmlAnyContain
        ".ui.comments > .comment > div > div.text"
        "Comment for Item1"

assertPostComment :: Bool -> YesodExample App ()
assertPostComment isAuthenticated = do
    (_, _, _, itemId) <- prepareScenarioWithoutComment
    request $ do
        setMethod "POST"
        addRequestHeader ("Content-Type", "application/json")
        let body = object ["comment" .= ("some comment" :: Text)]
        setRequestBody $ encode body
        if isAuthenticated
            then addGetParam "access_token" "bob--token"
            else return ()
        setUrl $ RestfulItemCommentsR itemId
    let status =
            if isAuthenticated
                then 201
                else 403
    statusIs status

prepareScenario ::
       YesodExample App ( Entity User
                        , CompanyId
                        , PdfFileId
                        , ItemId
                        , Entity User
                        , ItemCommentId)
prepareScenario = do
    currentTime <- liftIO getCurrentTime
    (john, companyId, pdfId, itemId) <- prepareScenarioWithoutComment
    alice <- createUser "alice"
    let (Entity commenterUserId _) = alice
    let commentText = "Comment for Item1"
    commentId <-
        runDB $
        insert $ ItemComment commentText itemId commenterUserId currentTime
    return (john, companyId, pdfId, itemId, alice, commentId)

prepareScenarioWithoutComment ::
       YesodExample App (Entity User, CompanyId, PdfFileId, ItemId)
prepareScenarioWithoutComment = do
    currentTime <- liftIO getCurrentTime
    john <- createUser "john"
    let (Entity userId user) = john
    companyId <- runDB $ insert $ Company "company1" currentTime userId
    pdfId <- runDB $ insert $ PdfFile "someFileName" currentTime
    itemId <-
        runDB $
        insert $ Item "Item1" companyId 10 (Just pdfId) currentTime userId
    return (john, companyId, pdfId, itemId)
