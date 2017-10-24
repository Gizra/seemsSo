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
import Utils.Item (ItemMeta(..))

getHomeR :: Handler Html
getHomeR = do
    muser <- maybeAuthPair
    -- Get recent 5 items.
    items <- runDB $ selectList [] [Desc ItemId, LimitTo 5]
    let itemsMeta =
            [ ItemMeta
            { itemMetaItem = Entity itemId item
            -- We don't need the company info for the links.
            , itemMetaCompany = Nothing
            , itemMetaPdfFilePath = Nothing
            }
            | (Entity itemId item) <- items
            ]
    let itemsMetaJson = encodeToLazyText $ toJSON itemsMeta
    -- Inject the general page.
    let elmFlags =
            encodeToLazyText $
            toJSON
                ElmWidgetFlags
                { elmWidgetFlagsPage = "homepage" :: Text
                , elmWidgetFlagsEntityId = Nothing
                }
    let userJson = encodeToLazyText $ maybe Null (toJSON . uncurry Entity) muser
    defaultLayout $ do
        setTitle "Welcome To SeemsSo!"
        addScript $ StaticR js_Main_js
        $(widgetFile "elm")
        $(widgetFile "home")
