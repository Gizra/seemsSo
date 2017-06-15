{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Data.Aeson.Text (encodeToLazyText)
import Import
import Text.Julius (rawJS)

data JsSettings = JsSettings
    { user :: Maybe (UserId, User)
    , items :: [Entity Item]
    }

instance ToJSON JsSettings where
    toJSON jsSettings = object ["user" .= userJson, "items" .= itemsJson]
      where
        userJson = maybe Null (toJSON . uncurry Entity) (user jsSettings)
        itemsJson = toJSON (items jsSettings)

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthPair
    -- Get recent 5 items.
    items <- runDB $ selectList [] [Desc ItemId]
    let jsSettings = encodeToLazyText JsSettings {user = muser, items = items}
    defaultLayout $ do
        setTitle "Welcome To SeemsSo!"
        $(widgetFile "js-settings")
        addScript $ StaticR js_Main_js
        -- @todo: Make widget type safe
        -- Inject the general page.
        let elmWidget = "homepage" :: Text
        $(widgetFile "elm")
        -- Inject page specific date.
        let elmValues = encodeToLazyText items
        $(widgetFile "home")
