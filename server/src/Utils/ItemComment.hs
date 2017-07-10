{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.ItemComment
    ( getEncodedItemComment
    , getEncodedItemCommentsByItemId
    , getItemComment
    , getItemCommentsByItemId
    ) where

import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Internal.Lazy as TL
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import Database.Persist.Sql (fromSqlKey)
import Import

data CommentWithUserInfo = CommentWithUserInfo
    { commentWithUserInfoCommentId :: ItemCommentId
    , commentWithUserInfoComment :: Text
    , commentWithUserInfoCommentCreated :: UTCTime
    , commentWithUserInfoUserId :: UserId
    , commentWithUserInfoUserIdent :: Text
    }

instance ToJSON CommentWithUserInfo where
    toJSON commentWithUserInfo =
        object
            [ "commentId" .= commentWithUserInfoCommentId commentWithUserInfo
            , "comment" .= commentWithUserInfoComment commentWithUserInfo
            , "created" .= commentWithUserInfoCommentCreated commentWithUserInfo
            , "userId" .= commentWithUserInfoUserId commentWithUserInfo
            , "name" .= commentWithUserInfoUserIdent commentWithUserInfo
            ]

getEncodedItemComment :: ItemCommentId -> Handler TL.Text
getEncodedItemComment itemCommentId = do
    commentsRaw <- getItemComment itemCommentId
    encodeItemComments commentsRaw

getEncodedItemCommentsByItemId :: ItemId -> Handler TL.Text
getEncodedItemCommentsByItemId itemId = do
    commentsRaw <- getItemCommentsByItemId itemId
    encodeItemComments commentsRaw

getItemCommentsByItemId ::
       ItemId
    -> Handler [( E.Value (ItemCommentId)
                , E.Value Text
                , E.Value UTCTime
                , E.Value (UserId)
                , E.Value Text)]
getItemCommentsByItemId itemId =
    runDB . E.select . E.from $ \(itemComment `E.InnerJoin` item `E.InnerJoin` user) -> do
        E.on $ user ^. UserId E.==. itemComment ^. ItemCommentUser
        E.on $ item ^. ItemId E.==. itemComment ^. ItemCommentItem
        E.where_ $ itemComment ^. ItemCommentItem E.==. E.val itemId
        E.orderBy [E.asc (itemComment ^. ItemCommentId)]
        E.limit 200
        return
            ( itemComment ^. ItemCommentId
            , itemComment ^. ItemCommentComment
            , itemComment ^. ItemCommentCreated
            , user ^. UserId
            , user ^. UserIdent)

getItemComment ::
       ItemCommentId
    -> Handler [( E.Value (ItemCommentId)
                , E.Value Text
                , E.Value UTCTime
                , E.Value (UserId)
                , E.Value Text)]
getItemComment itemCommentId =
    runDB . E.select . E.from $ \(itemComment `E.InnerJoin` item `E.InnerJoin` user) -> do
        E.on $ user ^. UserId E.==. itemComment ^. ItemCommentUser
        E.on $ item ^. ItemId E.==. itemComment ^. ItemCommentItem
        E.where_ $ itemComment ^. ItemCommentId E.==. E.val itemCommentId
        E.limit 1
        return
            ( itemComment ^. ItemCommentId
            , itemComment ^. ItemCommentComment
            , itemComment ^. ItemCommentCreated
            , user ^. UserId
            , user ^. UserIdent)

encodeItemComments ::
       [( E.Value ItemCommentId
        , E.Value Text
        , E.Value UTCTime
        , E.Value UserId
        , E.Value Text)]
    -> Handler TL.Text
encodeItemComments commentsRaw = do
    let commentWithUserInfos =
            [ CommentWithUserInfo
            { commentWithUserInfoCommentId = commentId
            , commentWithUserInfoComment = comment
            , commentWithUserInfoCommentCreated = commentCreated
            , commentWithUserInfoUserId = userId
            , commentWithUserInfoUserIdent = userIdent
            }
            | (E.Value commentId, E.Value comment, E.Value commentCreated, E.Value userId, E.Value userIdent) <-
                  commentsRaw
            ]
    return $ encodeToLazyText commentWithUserInfos
