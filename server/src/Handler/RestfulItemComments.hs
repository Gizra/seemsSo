{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulItemComments where

import Handler.RestfulItemComment (getRestfulItemCommentR)
import Import
import Network.Pusher (Channel(..), ChannelType(..), trigger)
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
    let commentText = semiItemCommentComment semiItemComment :: Text
    if null commentText
        then invalidArgs ["Comment text is required."]
        else do
            let itemComment =
                    ItemComment
                    { itemCommentComment = commentText
                    , itemCommentItem = itemId
                    , itemCommentUser = userId
                    , itemCommentCreated = currentTime
                    }
            itemCommentId <- runDB $ insert itemComment
            pusher <- fmap appPusher getYesod
            -- We don't care about the Pusher result.
            _ <-
                trigger
                    pusher
                    [Channel Public "my-channel"]
                    "my-event"
                    "my-data"
                    Nothing
            returnVal <- getRestfulItemCommentR itemId itemCommentId
            sendResponseStatus status201 returnVal
