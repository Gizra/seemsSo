{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Model where

import ClassyPrelude.Yesod
import Data.Aeson.Types
import Database.Persist.Quasi
import Database.Persist.Sql (fromSqlKey)
import Model.Types

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance ToJSON (Entity User) where
    toJSON (Entity userId user) =
        object ["id" .= fromSqlKey userId, "name" .= userIdent user]

instance ToJSON (Entity Order) where
    toJSON (Entity orderId order) =
        object ["id" .= fromSqlKey orderId, "status" .= orderStatus order]

instance ToJSON OrderStatus where
    toJSON = genericToJSON defaultOptions
