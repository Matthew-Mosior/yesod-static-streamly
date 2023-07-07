{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# Language QuasiQuotes       #-}

-- |
-- Module      :  Yesod.Static.Streamly
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Streamly-based alternative functionality for Yesod.Static.
--
-- This library utilizes [Streamly](https://hackage.haskell.org/package/streamly-core)'s superb performance characteristics to replace some of [Yesod](https://hackage.haskell.org/package/yesod)'s functionality with streamly-based functionality.

module Yesod.Static.Streamly ( -- * Yesod.Static Replacement functions
                               staticStreamly,
                               staticFilesStreamly
                             ) where

import Yesod.Static.Streamly.Internal

import Language.Haskell.TH
import Network.Wai.Application.Static (webAppSettingsWithLookup)
import Yesod.Static

-- | A more performant replacement of
-- [static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:static)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticStreamly :: FilePath
               -> IO Static
staticStreamly dir = do
  hashLookup <- cachedETagLookupStreamly dir
  return $ Static $ webAppSettingsWithLookup dir hashLookup

-- | A more performant replacement of
-- [staticFiles](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#staticFiles)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticFilesStreamly :: FilePath
                    -> Q [Dec]
staticFilesStreamly dir = mkStaticFilesStreamly dir
