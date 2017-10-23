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
import Data.Char as Char (toLower)
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

instance ToJSON (Entity Item) where
    toJSON (Entity entityId entity) =
        object
            [ "id" .= (fromSqlKey entityId)
            , "name" .= itemName entity
            , "created" .= itemCreated entity
            , "user" .= itemUser entity
            , "price" .= itemPrice entity
            ]

instance ToJSON (Entity Company) where
    toJSON (Entity entityId entity) =
        object
            [ "id" .= (fromSqlKey entityId)
            , "name" .= companyName entity
            , "created" .= companyCreated entity
            , "user" .= companyUser entity
            ]

-- @todo: Use orderStatusLabel
instance ToJSON OrderStatus where
    toJSON a = String $ pack (map Char.toLower (drop 11 $ show a))

instance FromJSON OrderStatus where
    parseJSON (Object o) = do
        status <- o .: "status"
        case (status :: Text) of
            "active" -> return OrderStatusActive
            "cancelled" -> return OrderStatusCancelled
            -- "Payment Error" and "Paid" should never change from the client
            -- so we simply don't decode them,
            _ -> mzero
    parseJSON invalid = typeMismatch "status" invalid
