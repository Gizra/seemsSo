{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Data.Aeson.Text (encodeToLazyText)
import Import
import Text.Julius (rawJS)
import Utils.Elm (ElmWidgetFlags(..))

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthPair
    -- Get recent 5 items.
    items <- runDB $ selectList [] [Desc ItemId]
    defaultLayout $ do
        setTitle "Welcome To SeemsSo!"
        addScript $ StaticR js_Main_js
        -- Inject the general page.
        let elmFlags =
                encodeToLazyText $
                toJSON
                    ElmWidgetFlags
                    { elmWidgetFlagsPage = "homepage" :: Text
                    , elmWidgetFlagsEntityId = Nothing
                    }
        let userJson =
                encodeToLazyText $ maybe Null (toJSON . uncurry Entity) muser
        $(widgetFile "elm")
        -- Inject page specific data.
        let itemsJson = encodeToLazyText items
        $(widgetFile "home")
