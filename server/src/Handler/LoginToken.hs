{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.LoginToken where

import Import

getLoginTokenR :: Handler Value
getLoginTokenR = do
    uid <- requireAuthId
    mAccessToken <- runDB $ selectFirst [AccessTokenUserId ==. uid] []
    returnJson mAccessToken
