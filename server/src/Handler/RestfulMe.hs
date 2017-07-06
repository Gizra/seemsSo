{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.RestfulMe where

import Import
import Utils.Restful

getRestfulMeR :: Handler Value
getRestfulMeR = do
    (uid, user) <- requireAuthPair
    return $ object ["data" .= toJSON (Entity uid user)]
