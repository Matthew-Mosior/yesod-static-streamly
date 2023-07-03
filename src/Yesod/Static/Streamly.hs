{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Yesod.Static.Streamly
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Streamly-based static file functions.
--
-- This library attempts to utilize [Streamly](https://hackage.haskell.org/package/streamly-core)'s superb performance characteristics to replace some of [Yesod](https://hackage.haskell.org/package/yesod) functionality with streamly-based functionality.

module Yesod.Static.Streamly ( -- * Yesod.Default.Util Replacement function
                               addStaticContentExternalStreamly 
                             ) where

import Control.Monad (unless)
import Data.ByteString.Lazy as L (ByteString)
import Data.Text as DText (Text,pack,unpack)
import Streamly.External.ByteString.Lazy as StreamlyLByteString
import qualified Streamly.Internal.FileSystem.File as StreamlyFile
import System.Directory (doesFileExist,createDirectoryIfMissing)
import Yesod.Core

-- | A more performant replacement of
-- [addStaticContentExternal](https://hackage.haskell.org/package/yesod-1.6.2.1/docs/Yesod-Default-Util.html#v:addStaticContentExternal)
-- found in [Yesod.Default.Util](https://hackage.haskell.org/package/yesod-1.6.2.1/docs/Yesod-Default-Util.html).
addStaticContentExternalStreamly :: (L.ByteString -> Either a L.ByteString) -- ^ javascript minifier
                                 -> (L.ByteString -> String) -- ^ hash function to determine file name
                                 -> FilePath -- ^ location of static directory. files will be placed within a "tmp" subfolder
                                 -> ([Text] -> Route master) -- ^ route constructor, taking a list of pieces
                                 -> Text -- ^ filename extension
                                 -> Text -- ^ mime type
                                 -> L.ByteString -- ^ file contents
                                 -> HandlerFor master (Maybe (Either Text (Route master, [(Text, Text)])))
addStaticContentExternalStreamly minify hash staticDir toRoute ext' _ content = do
    liftIO $ createDirectoryIfMissing True statictmp
    exists <- liftIO $ doesFileExist fn'
    unless exists $
      liftIO $ StreamlyFile.fromChunks fn'
                                       (StreamlyLByteString.toChunks content')
    return $ Just $ Right (toRoute ["tmp", DText.pack fn], [])
  where
    fn, statictmp, fn' :: FilePath
    -- by basing the hash off of the un-minified content, we avoid a costly
    -- minification if the file already exists
    fn = hash content ++ '.' : unpack ext'
    statictmp = staticDir ++ "/tmp/"
    fn' = statictmp ++ fn

    content' :: L.ByteString
    content'
        | ext' == "js" = either (const content) id $ minify content
        | otherwise = content
