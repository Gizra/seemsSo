{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        setTitle "Welcome To SeemsSo!"
        addScript $ StaticR js_Main_js
        addScript $ StaticR js_app_js
