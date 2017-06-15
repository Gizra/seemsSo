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
    }

instance ToJSON JsSettings where
    toJSON jsSettings = object ["user" .= userJson]
      where
        userJson =
            maybe Null (\user -> toJSON $ uncurry Entity user) (user jsSettings)

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthPair
    let jsSettings = encodeToLazyText JsSettings {user = muser}
    defaultLayout $ do
        setTitle "Welcome To SeemsSo!"
        $(widgetFile "js-settings")
        addScript $ StaticR js_Main_js
        addScript $ StaticR js_app_js
