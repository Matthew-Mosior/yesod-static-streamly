module Main (main) where

import Yesod.Default.Util.Streamly
import Yesod.Static.Streamly
import Yesod.Static.Streamly.Internal

import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import qualified Data.ByteArray as ByteArray
import Test.Hspec
import Control.Exception (evaluate)
import Crypto.Hash
import Crypto.Hash.Conduit (hashFile)
import Yesod.Static

main :: IO ()
main = hspec $ do
  describe "Yesod.Static.Streamly.Internal" $ do
    describe "hashFileStreamly" $ do
      it "Ensures that hashFileStreamly is equivalent to Crypto.Hash.Conduit's hashFile function." $ do
        hashfilestreamlyt <- hashFileStreamly "test/hashfilestreamlytest.txt" 100
        let hashfilestreamlytf = ((ByteArray.convert (hashfilestreamlyt :: Crypto.Hash.Digest MD5)) :: BS.ByteString)
        hashfilet <- hashFile "test/hashfilestreamlytest.txt"
        let hashfiletf = ((ByteArray.convert (hashfilet :: Crypto.Hash.Digest MD5)) :: BS.ByteString)
        hashfilestreamlytf `shouldBe` hashfiletf  
    describe "base64md5Streamly" $ do
      it "Ensures that base64md5Streamly is equivalent to Yesod.Static's base64md5 function." $ do
         base64md5bst <- BSL.readFile "test/hashfilestreamlytest.txt"
         let base64md5streamlyt = base64md5Streamly base64md5bst
         let base64md5t = base64md5 base64md5bst
         base64md5streamlyt `shouldBe` base64md5t
