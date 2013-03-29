{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import System.IO.Unsafe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as Err
import qualified Data.ByteString as BS
import System.Environment

import Data.Monoid (mconcat)

contents = T.pack . show . (E.decodeUtf8With Err.lenientDecode) . unsafePerformIO $ BS.readFile "src/blogpost.html"

main = do
  port <- getEnv "PORT"
  scotty (fromIntegral $ read port) $ do
    get "/" $ do
      html contents
  
    get "/:word" $ do
      beam <- param "word"      
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
