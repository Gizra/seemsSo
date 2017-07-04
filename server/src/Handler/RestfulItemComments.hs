{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulItemComments where

import Handler.RestfulItemComment (getRestfulItemCommentR)
import Import
import Utils.Restful (getEntityList)

-- A Bid type that represents the data we will get from JSON.
data SemiItemComment = SemiItemComment
    { semiItemCommentComment :: Text
    }

instance FromJSON SemiItemComment where
    parseJSON (Object v) = SemiItemComment <$> v .: "comment"
    parseJSON _ = mzero

getRestfulItemCommentsR :: ItemId -> Handler Value
getRestfulItemCommentsR itemId =
    getEntityList
        (RestfulItemCommentsR itemId)
        (RestfulItemCommentR itemId)
        [ItemCommentItem ==. itemId]

postRestfulItemCommentsR :: ItemId -> Handler Value
postRestfulItemCommentsR itemId = do
    currentTime <- liftIO getCurrentTime
    userId <- requireAuthId
    semiItemComment <- requireJsonBody :: Handler SemiItemComment
    let itemComment =
            ItemComment
            { itemCommentComment = semiItemCommentComment semiItemComment
            , itemCommentItem = itemId
            , itemCommentUser = userId
            , itemCommentCreated = currentTime
            }
    itemCommentId <- runDB $ insert itemComment
    returnVal <- getRestfulItemCommentR itemId itemCommentId
    sendResponseStatus status201 returnVal
