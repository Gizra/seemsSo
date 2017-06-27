{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Form
    ( renderSematnicUiDivs
    , selectSettings
    ) where

import Import

-- Adaptation of renderDivs.
renderSematnicUiDivs :: Monad m => FormRender m a
renderSematnicUiDivs = renderSematnicUiDivsMaybeLabels True

-- Only difference here is that we add a ".field" class on the wrapper div.
renderSematnicUiDivsMaybeLabels :: Monad m => Bool -> FormRender m a
renderSematnicUiDivsMaybeLabels withLabels aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
    let widget =
            [whamlet|
                    $newline never
                    \#{fragment}
                    $forall view <- views
                      <div.field :fvRequired view:.required :not $ fvRequired view:.optional>
                          $if withLabels
                                  <label for=#{fvId view}>#{fvLabel view}
                          $maybe tt <- fvTooltip view
                              <div .tooltip>#{tt}
                          ^{fvInput view}
                          $maybe err <- fvErrors view
                              <div .errors>#{err}
                    |]
    return (res, widget)

selectSettings label =
    FieldSettings
    { fsLabel = label
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs = [("class", "ui fluid dropdown")]
    }
