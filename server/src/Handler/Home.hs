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
    { foo :: Text
    }

instance ToJSON JsSettings where
    toJSON jsSettings = object ["foo" .= foo jsSettings]

getHomeR :: Handler Html
getHomeR = do
    let jsSettings = encodeToLazyText JsSettings {foo = "bar"}
    defaultLayout $ do
        setTitle "Welcome To SeemsSo!"
        $(widgetFile "js-settings")
        addScript $ StaticR js_Main_js
        addScript $ StaticR js_app_js
