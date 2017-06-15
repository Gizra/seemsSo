{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Data.Aeson.Text (encodeToLazyText)
import Import
import Text.Julius (rawJS)

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthPair
    -- Get recent 5 items.
    items <- runDB $ selectList [] [Desc ItemId]
    defaultLayout $ do
        setTitle "Welcome To SeemsSo!"
        addScript $ StaticR js_Main_js
        -- @todo: Make widget type safe
        -- Inject the general page.
        let elmWidget = "homepage" :: Text
        let userJson =
                encodeToLazyText $
                maybe Null (toJSON . uncurry Entity) muser
        $(widgetFile "elm")
        -- Inject page specific date.
        let itemsJson = encodeToLazyText items
        $(widgetFile "home")
