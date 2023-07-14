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
--
-- If you have large files to cache within your static directory, you may very well need to increase you file descriptor limit in order to utilize the functionality this library provides properly.

module Yesod.Static.Streamly ( -- * Yesod.Static Replacement functions - Smart constructor
                               staticStreamly,
                               staticDevelStreamly,
                               -- * Yesod.Static Replacement functions - Template Haskell helpers
                               staticFilesStreamly,
                               staticFilesListStreamly,
                               staticFilesMapStreamly,
                               staticFilesMergeMapStreamly,
                               publicFilesStreamly
                             ) where

import Yesod.Static.Streamly.Internal

import Data.List (foldl')
import qualified Data.Map as M
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import Network.Wai.Application.Static (webAppSettingsWithLookup)
import qualified System.FilePath as FP
import Yesod.Static

-- | A more performant replacement of
-- [static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:static)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticStreamly :: FilePath -- ^ file path of static directory
               -> Int      -- ^ buffer size (0.25 - 0.50 x your L2 cache seems to be best.)
               -> IO Static
staticStreamly dir size = do
  hashLookup <- cachedETagLookupStreamly dir
                                         size
  return $ Static $ webAppSettingsWithLookup dir hashLookup

-- | A more performant replacement of
-- [staticDevel](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:staticDevel)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticDevelStreamly :: FilePath -- ^ file path of static directory
                    -> Int      -- ^ buffer size (0.25 - 0.50 x your L2 cache seems to be best.)
                    -> IO Static
staticDevelStreamly dir size = do
  hashLookup <- cachedETagLookupDevelStreamly dir
                                              size
  return $ Static $ webAppSettingsWithLookup dir hashLookup

-- | A more performant replacement of
-- [staticFiles](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:staticFiles)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticFilesStreamly :: FilePath -- ^ file path of static directory
                    -> Int      -- ^ buffer size (0.25 - 0.50 x your L2 cache seems to be best.)
                    -> Q [Dec]
staticFilesStreamly dir size = mkStaticFilesStreamly dir
                                                     size

-- | A more performant replacement of
-- [staticFilesList](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:staticFilesList)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticFilesListStreamly :: FilePath -- ^ file path of static directory
                        -> [FilePath]
                        -> Int      -- ^ buffer size (0.25 - 0.50 x your L2 cache seems to be best.)
                        -> Q [Dec]
staticFilesListStreamly dir fs size = 
  mkStaticFilesListStreamly dir
                            (map split fs)
                            True
                            size
    where
      split :: FilePath
            -> [String]
      split [] = []
      split x = let (a, b) = break (== '/') x
                  in a : split (drop 1 b)

-- | A more performant replacement of
-- [staticFilesMap](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:staticFilesMap)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticFilesMapStreamly :: FilePath -- ^ file path of static directory
                       -> M.Map FilePath FilePath 
                       -> Int      -- ^ buffer size (0.25 - 0.50 x your L2 cache seems to be best.)
                       -> Q [Dec]
staticFilesMapStreamly fp m size =
  mkStaticFilesListStreamly' fp
                             (map splitBoth mapList)
                             True
                             size
    where
      splitBoth (k, v) = (split k, split v)
      mapList = M.toList m
      split :: FilePath
            -> [String]
      split [] = []
      split x = let (a, b) = break (== '/') x
                  in a : split (drop 1 b)

-- | A more performant replacement of
-- [staticFilesMergeMap](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:staticFilesMergeMap)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
staticFilesMergeMapStreamly :: FilePath -- ^ file path of static directory
                            -> M.Map FilePath FilePath
                            -> Int      -- ^ buffer size (0.25 - 0.50 x your L2 cache seems to be best.)
                            -> Q [Dec]
staticFilesMergeMapStreamly fp m size = do
  fs <- qRunIO $ getFileListPiecesStreamly fp
  let filesList     = map FP.joinPath fs
      mergedMapList = M.toList $ foldl' (checkedInsert invertedMap) m filesList
  mkStaticFilesListStreamly' fp
                             (map splitBoth mergedMapList)
                             True
                             size
    where
      splitBoth (k,v) = (split k, split v)
      swap (x,y)      = (y, x)
      mapList         = M.toList m
      invertedMap     = M.fromList $ map swap mapList
      split :: FilePath
            -> [String]
      split [] = []
      split x = let (a, b) = break (== '/') x
                  in a : split (drop 1 b)
      -- We want to keep mappings for all files that are pre-fingerprinted,
      -- so this function checks against all of the existing fingerprinted files and
      -- only inserts a new mapping if it's not a fingerprinted file.
      checkedInsert
        :: M.Map FilePath FilePath -- inverted dictionary
        -> M.Map FilePath FilePath -- accumulating state
        -> FilePath
        -> M.Map FilePath FilePath
      checkedInsert iDict st p = if M.member p iDict
                                   then st
                                   else M.insert p p st

-- | A more performant replacement of
-- [publicFiles](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:publicFiles)
-- found in [Yesod.Static](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html).
publicFilesStreamly :: FilePath -- ^ file path of static directory
                    -> Int      -- ^ buffer size (0.25 - 0.50 x your L2 cache seems to be best.)
                    -> Q [Dec]
publicFilesStreamly dir size = mkStaticFilesStreamly' dir
                                                      False
                                                      size
