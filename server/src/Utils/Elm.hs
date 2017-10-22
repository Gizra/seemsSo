{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Utils.Elm
    ( ElmWidgetFlags(..)
    ) where

import Import.NoFoundation



{-| The flags we're going to send into the Elm app.
-} 
data ElmWidgetFlags = ElmWidgetFlags
    { elmWidgetFlagsPage :: Text
    , elmWidgetFlagsEntityId :: Maybe Int
    }

instance ToJSON ElmWidgetFlags where
    toJSON elmWidgetFlags =
        object
            [ "page" .= elmWidgetFlagsPage elmWidgetFlags
            , "entityId" .= elmWidgetFlagsEntityId elmWidgetFlags
            ]
