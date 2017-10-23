{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Item
    ( ItemMeta(..)
    ) where

import Import

{-| An extended version of `Item` along with all the related data (e.g. company, and images).
-}
data ItemMeta = ItemMeta
    { itemMetaItem :: Entity Item
    , itemMetaCompany :: Maybe (Entity Company)
    }

instance ToJSON ItemMeta where
    toJSON itemMeta =
        object
            [ "item" .= itemMetaItem itemMeta
            , "company" .= itemMetaCompany itemMeta
            ]
