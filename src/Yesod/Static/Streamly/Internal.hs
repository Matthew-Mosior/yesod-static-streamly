{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# Language QuasiQuotes           #-}

-- |
-- Module      :  Yesod.Static.Streamly.Internal
-- Copyright   :  (c) Matthew Mosior 2023
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this library are expected to track development
-- closely.
--
-- All credit goes to the author(s)/maintainer(s) of the
-- [containers](https://hackage.haskell.org/package/containers) library
-- for the above warning text.
--
-- = Description
--
-- This library utilizes [Streamly](https://hackage.haskell.org/package/streamly-core)'s superb performance characteristics to replace some of [Yesod](https://hackage.haskell.org/package/yesod)'s functionality with streamly-based functionality.

module Yesod.Static.Streamly.Internal ( -- * Yesod.Static Replacement functions (INTERNAL)
                                        mkStaticFilesStreamly,
                                        mkStaticFilesStreamly',
                                        mkStaticFilesListStreamly,
                                        mkStaticFilesListStreamly',
                                        cachedETagLookupDevelStreamly,
                                        cachedETagLookupStreamly,
                                        mkHashMapStreamly,
                                        notHiddenStreamly,
                                        getFileListPiecesStreamly,
                                        pathFromRawPiecesStreamly,
                                        base64md5FileStreamly,
                                        base64Streamly,
                                        hashFileStreamly,
                                        sinkHashStreamly
                                      ) where

import Control.Monad.State.Strict
import "cryptonite" Crypto.Hash (hashlazy,Digest,MD5)
import "cryptonite" Crypto.Hash.IO (HashAlgorithm)
import qualified Data.ByteArray as ByteArray
import Data.ByteString as B (ByteString)
import Data.ByteString.Lazy as L (ByteString)
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import Data.Char (isLower,isDigit)
import Data.IORef (readIORef,newIORef,writeIORef)
import Data.List (foldl',intercalate,sort)
import qualified Data.Map as M
import Data.Text (pack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import qualified Streamly.Data.Stream as S
import Streamly.External.ByteString.Lazy as StreamlyLByteString (fromChunksIO)
import Streamly.Internal.FileSystem.File as StreamlyInternalFile (chunkReaderWith)
import Streamly.Internal.System.IO (arrayPayloadSize)
import System.Directory (doesDirectoryExist,doesFileExist,getDirectoryContents)
import System.PosixCompat.Files (getFileStatus,modificationTime)
import System.Posix.Types (EpochTime)
import WaiAppStatic.Storage.Filesystem (ETagLookup)
import Yesod.Static

-- | A replacement of
-- [mkStaticFiles](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#mkStaticFiles).
mkStaticFilesStreamly :: FilePath
                      -> Int 
                      -> Q [Dec]
mkStaticFilesStreamly fp size = mkStaticFilesStreamly' fp True size

-- | A replacement of
-- [mkStaticFiles'](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#mkStaticFiles').
mkStaticFilesStreamly' :: FilePath -- ^ static directory
                       -> Bool     -- ^ append checksum query parameter
                       -> Int      -- ^ buffer size
                       -> Q [Dec]
mkStaticFilesStreamly' fp makeHash size = do
  fs <- qRunIO $ getFileListPiecesStreamly fp
  mkStaticFilesListStreamly fp fs makeHash size

-- | A replacement of
-- [mkStaticFilesList](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#mkStaticFilesList).
mkStaticFilesListStreamly :: FilePath   -- ^ static directory
                          -> [[String]] -- ^ list of files to create identifiers for
                          -> Bool       -- ^ append checksum query parameter
                          -> Int        -- ^ buffer size
                          -> Q [Dec]
mkStaticFilesListStreamly fp fs makeHash size = mkStaticFilesListStreamly' fp (zip fs fs) makeHash size

-- | A replacement of
-- [mkStaticFilesList'](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#mkStaticFilesList').
mkStaticFilesListStreamly' :: FilePath               -- ^ static directory
                           -> [([String], [String])] -- ^ list of files to create identifiers for, where
                                                     -- the first argument of the tuple is the identifier
                                                     -- alias and the second is the actual file name
                           -> Bool                   -- ^ append checksum query parameter
                           -> Int                    -- ^ buffer size
                           -> Q [Dec]
mkStaticFilesListStreamly' fp fs makeHash size =
  concat `fmap` mapM mkRoute fs
    where
      replace' c
          | 'A' <= c && c <= 'Z' = c
          | 'a' <= c && c <= 'z' = c
          | '0' <= c && c <= '9' = c
          | otherwise = '_'
      mkRoute (alias,f) = do let name' = Data.List.intercalate "_" $ map (map replace') alias
                                 routeName = mkName $
                                     case () of
                                         ()
                                             | Prelude.null name' -> error "null-named file"
                                             | isDigit (head name') -> '_' : name'
                                             | isLower (head name') -> name'
                                             | otherwise -> '_' : name'
                             f' <- [|map pack $(TH.lift f)|]
                             qs <- if makeHash
                                     then do hash <- qRunIO $ base64md5FileStreamly (pathFromRawPiecesStreamly fp f)
                                                                                    size
                                             [|[(pack "etag",pack $(TH.lift hash))]|]
                                     else return $ ListE []
                             return
                                 [ SigD routeName $ ConT ''StaticRoute
                                 , FunD routeName
                                     [ Clause [] (NormalB $ (ConE 'StaticRoute) `AppE` f' `AppE` qs) []
                                     ]
                                 ]

-- | A replacement of
-- [cachedETagLookupDevel](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#cachedETagLookupDevel).
cachedETagLookupDevelStreamly :: FilePath
                              -> Int
                              -> IO ETagLookup
cachedETagLookupDevelStreamly dir size = do
  etags <- mkHashMapStreamly dir
                             size
  mtimeVar <- newIORef (M.empty :: M.Map FilePath EpochTime)
  return $ \f ->
    case M.lookup f etags of
      Nothing       -> return Nothing
      Just checksum -> do
        fs <- getFileStatus f
        let newt = modificationTime fs
        mtimes <- readIORef mtimeVar
        oldt <- case M.lookup f mtimes of
          Nothing -> writeIORef mtimeVar (M.insert f newt mtimes) >> return newt
          Just oldt -> return oldt
        return $ if newt /= oldt then Nothing else Just checksum

-- | A replacement of
-- [cachedETagLookup](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#cachedETagLookup).
cachedETagLookupStreamly :: FilePath
                         -> Int
                         -> IO ETagLookup
cachedETagLookupStreamly dir size = do
  etags <- mkHashMapStreamly dir
                             size
  return $ (\f -> return $ M.lookup f etags)

-- | A replacement of
-- [mkHashMap](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#mkHashMap).
mkHashMapStreamly :: FilePath
                  -> Int
                  -> IO (M.Map FilePath S8.ByteString)
mkHashMapStreamly dir size = do
  fs <- getFileListPiecesStreamly dir
  hashAlist fs >>= return . M.fromList
    where
      hashAlist :: [[String]]
                -> IO [(FilePath,S8.ByteString)]
      hashAlist fs = mapM hashPair fs
          where
            hashPair :: [String]
                     -> IO (FilePath,S8.ByteString)
            hashPair pieces = do let file = pathFromRawPiecesStreamly dir pieces
                                 h <- base64md5FileStreamly file
                                                            size
                                 return (file,S8.pack h)

-- | A replacement of
-- [notHidden](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#notHidden).
notHiddenStreamly :: FilePath
                  -> Bool
notHiddenStreamly "tmp" = False
notHiddenStreamly s     =
  case s of
    '.':_ -> False
    _     -> True

-- | A replacement of
-- [getFileListPieces](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#getFileListPieces).
getFileListPiecesStreamly :: FilePath
                          -> IO [[String]]
getFileListPiecesStreamly = flip evalStateT M.empty . flip go id
  where
    go :: String
       -> ([String] -> [String])
       -> StateT (M.Map String String) IO [[String]]
    go fp front = do
      allContents <- liftIO $ (sort . Prelude.filter notHiddenStreamly) `fmap` getDirectoryContents fp
      let fullPath :: String -> String
          fullPath f = fp ++ '/' : f
      files <- liftIO $ filterM (doesFileExist . fullPath) allContents
      let files' = map (front . return) files
      files'' <- mapM dedupe files'
      dirs <- liftIO $ filterM (doesDirectoryExist . fullPath) allContents
      dirs' <- mapM (\f -> go (fullPath f) (front . (:) f)) dirs
      return $ concat $ files'' : dirs'
    
    -- Reuse data buffers for identical strings
    dedupe :: [String]
           -> StateT (M.Map String String) IO [String]
    dedupe = mapM dedupe'

    dedupe' :: String
            -> StateT (M.Map String String) IO String
    dedupe' s = do
      m <- get
      case M.lookup s m of
        Just s' -> return s'
        Nothing -> do put $ M.insert s s m
                      return s

-- | A replacement of
-- [pathFromRawPieces](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#pathFromRawPieces).
pathFromRawPiecesStreamly :: FilePath
                          -> [String]
                          -> FilePath
pathFromRawPiecesStreamly =
  foldl' append
    where
      append a b = a ++ '/' : b

-- | A replacement of
-- [base64md5File](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#base64md5File).
base64md5FileStreamly :: FilePath
                      -> Int
                      -> IO String
base64md5FileStreamly fp size = fmap (base64Streamly . encode) $ (hashFileStreamly fp size)
    where encode d = ByteArray.convert (d :: Crypto.Hash.Digest MD5)

-- | A replacement of
-- [base64](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#base64).
base64Streamly :: B.ByteString
               -> String
base64Streamly = map tr
       . Prelude.take 8
       . S8.unpack
       . Data.ByteString.Base64.encode
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c   = c

-- | A more performant replacement of
-- [hashFile](https://hackage.haskell.org/package/cryptohash-conduit-0.1.1/docs/src/Crypto-Hash-Conduit.html#hashFile)
-- found in [Crypto.Hash.Conduit](https://hackage.haskell.org/package/cryptohash-conduit-0.1.1/docs/Crypto-Hash-Conduit.html).
hashFileStreamly :: ( MonadIO m
                    , Crypto.Hash.IO.HashAlgorithm hash
                    )
                 => FilePath
                 -> Int
                 -> m (Crypto.Hash.Digest hash)
hashFileStreamly fp size = do
  let lazyfile = S.unfold StreamlyInternalFile.chunkReaderWith (arrayPayloadSize (size * 1024),fp)
  lazyfilef <- liftIO $ StreamlyLByteString.fromChunksIO lazyfile
  sinkHashStreamly lazyfilef
  
-- | A more performant replacement of
-- [sinkHash](https://hackage.haskell.org/package/cryptohash-conduit-0.1.1/docs/src/Crypto-Hash-Conduit.html#sinkHash)
-- found in [Crypto.Hash.Conduit](https://hackage.haskell.org/package/cryptohash-conduit-0.1.1/docs/Crypto-Hash-Conduit.html).
sinkHashStreamly :: ( Monad m
                    , Crypto.Hash.IO.HashAlgorithm hash
                    )
                 => L.ByteString
                 -> m (Crypto.Hash.Digest hash)
sinkHashStreamly blcontent = return $ hashlazy blcontent
