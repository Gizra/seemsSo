{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Types where

import Database.Persist.TH
import GHC.Generics
import Prelude

data OrderStatus
    = OrderStatusActive
    | OrderStatusCancelled
    | OrderStatusPaymentError
    | OrderStatusPaid
    deriving (Show, Eq, Enum, Bounded, Read, Generic)

derivePersistField "OrderStatus"
