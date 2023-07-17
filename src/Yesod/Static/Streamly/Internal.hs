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
                                        CombineTypeStreamly(..),
                                        CombineSettingsStreamly(..),
                                        liftRoutesStreamly,
                                        combineStaticsStreamly',
                                        base64md5FileStreamly,
                                        base64md5Streamly,
                                        base64Streamly,
                                        hashFileStreamly,
                                        sinkHashStreamly
                                      ) where

import Control.Monad.State.Strict
import "cryptonite" Crypto.Hash (hashlazy,Digest,MD5)
import "cryptonite" Crypto.Hash.IO (HashAlgorithm)
import qualified Data.ByteArray as ByteArray
import Data.ByteString as B (ByteString)
import Data.ByteString.Lazy as L (ByteString,concat,writeFile)
import qualified Data.ByteString.Base64
import qualified Data.ByteString.Char8 as S8
import Data.Char (isLower,isDigit)
import Data.Default
import Data.IORef (readIORef,newIORef,writeIORef)
import Data.List (foldl',intercalate,sort)
import qualified Data.Map as M
--import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Language.Haskell.TH
import Language.Haskell.TH.Syntax as TH
import qualified Streamly.Data.Stream as S
import Streamly.External.ByteString.Lazy as StreamlyLByteString (fromChunksIO)
import Streamly.Internal.FileSystem.File as StreamlyInternalFile (chunkReaderWith)
import Streamly.Internal.System.IO (arrayPayloadSize)
import System.Directory (createDirectoryIfMissing,doesDirectoryExist,doesFileExist,getDirectoryContents)
import System.FilePath ((</>), (<.>), takeDirectory)
import qualified System.FilePath as F
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
  Prelude.concat `fmap` mapM mkRoute fs
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
      return $ Prelude.concat $ files'' : dirs'
    
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
-- [CombineType](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#CombineType).
data CombineTypeStreamly = JS | CSS

-- | A replacement of
-- [CombineSettings](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#t:CombineSettings).
data CombineSettingsStreamly = CombineSettingsStreamly
    { csStaticDir :: FilePath
    -- ^ File path containing static files.
    --
    -- Default: static
    --
    -- Since 1.2.0
    , csCssPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
    -- ^ Post processing to be performed on CSS files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csJsPostProcess :: [FilePath] -> L.ByteString -> IO L.ByteString
    -- ^ Post processing to be performed on Javascript files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csCssPreProcess :: TL.Text -> IO TL.Text
    -- ^ Pre-processing to be performed on CSS files.
    --
    -- Default: convert all occurences of /static/ to ../
    --
    -- Since 1.2.0
    , csJsPreProcess :: TL.Text -> IO TL.Text
    -- ^ Pre-processing to be performed on Javascript files.
    --
    -- Default: Pass-through.
    --
    -- Since 1.2.0
    , csCombinedFolder :: FilePath
    -- ^ Subfolder to put combined files into.
    --
    -- Default: combined
    --
    -- Since 1.2.0
    }

instance Default CombineSettingsStreamly where
    def = CombineSettingsStreamly
        { csStaticDir = "static"
        {- Disabled due to: https://github.com/yesodweb/yesod/issues/623
        , csCssPostProcess = \fps ->
              either (error . (errorIntro fps)) (return . TLE.encodeUtf8)
            . flip luciusRTMinified []
            . TLE.decodeUtf8
        -}
        , csCssPostProcess = const return
        , csJsPostProcess = const return
           -- FIXME The following borders on a hack. With combining of files,
           -- the final location of the CSS is no longer fixed, so relative
           -- references will break. Instead, we switched to using /static/
           -- absolute references. However, when served from a separate domain
           -- name, this will break too. The solution is that, during
           -- development, we keep /static/, and in the combining phase, we
           -- replace /static with a relative reference to the parent folder.
        , csCssPreProcess =
              return
            . TL.replace "'/static/" "'../"
            . TL.replace "\"/static/" "\"../"
        , csJsPreProcess = return
        , csCombinedFolder = "combined"
        }

-- | A replacement of
-- [liftRoutes](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#liftRoutes).
liftRoutesStreamly :: [Route Static]
                   -> Q Exp
liftRoutesStreamly =
  fmap ListE . mapM go
    where
      go :: Route Static -> Q Exp
      go (StaticRoute x y) = [|StaticRoute $(liftTexts x) $(liftPairs y)|]

      liftTexts = fmap ListE . mapM liftT
      liftT t = [|pack $(TH.lift $ T.unpack t)|]

      liftPairs = fmap ListE . mapM liftPair
      liftPair (x, y) = [|($(liftT x), $(liftT y))|]

-- | A replacement of
-- [combineStatics'](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#combineStatics').
combineStaticsStreamly' :: CombineTypeStreamly
                        -> CombineSettingsStreamly
                        -> [Route Static] -- ^ files to combine
                        -> Int                
                        -> Q Exp
combineStaticsStreamly' combineType CombineSettingsStreamly {..} routes size = do
  texts       <- qRunIO $ mapM (\fp -> do let lazyfile = S.unfold StreamlyInternalFile.chunkReaderWith (arrayPayloadSize (size * 1024),fp)
                                          lazybs       <- liftIO $ StreamlyLByteString.fromChunksIO lazyfile
                                          return lazybs
                               )
                          fps
  let textss  = L.concat texts
  let textsss = TLE.decodeUtf8 textss
  ltext       <- qRunIO $ preProcess textsss
  bs          <- qRunIO $ postProcess fps $ TLE.encodeUtf8 ltext
  let hash'   = base64md5Streamly bs
      suffix  = csCombinedFolder </> hash' <.> extension
      fp      = csStaticDir </> suffix
  qRunIO $ do
      createDirectoryIfMissing True $ takeDirectory fp
      L.writeFile fp bs
  let pieces = map T.unpack $ T.splitOn "/" $ T.pack suffix
  [|StaticRoute (map pack pieces) []|]
    where
      fps :: [FilePath]
      fps = map toFP routes
      toFP (StaticRoute pieces _) = csStaticDir </> F.joinPath (map T.unpack pieces)
      postProcess =
          case combineType of
              JS -> csJsPostProcess
              CSS -> csCssPostProcess
      preProcess =
          case combineType of
              JS -> csJsPreProcess
              CSS -> csCssPreProcess
      extension =
          case combineType of
              JS -> "js"
              CSS -> "css"

-- | A replacement of
-- [base64md5File](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/src/Yesod.Static.html#base64md5File).
base64md5FileStreamly :: FilePath
                      -> Int
                      -> IO String
base64md5FileStreamly fp size = fmap (base64Streamly . encode) $ (hashFileStreamly fp size)
    where encode d = ByteArray.convert (d :: Crypto.Hash.Digest MD5)

-- | A replacement of
-- [base64md5](https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-Static.html#v:base64md5).
base64md5Streamly :: L.ByteString
                  -> String
base64md5Streamly lbs = do
  let hashedlbs = hashlazy lbs
  base64Streamly $ encode hashedlbs
    where
      encode d = ByteArray.convert (d :: Digest MD5)

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
