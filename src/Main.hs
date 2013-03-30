{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, EmptyDataDecls #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Time
import qualified Web.Scotty as S
import System.IO.Unsafe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as Err
import qualified Data.ByteString as BS
import System.Environment
import Obvious.Model
import Obvious.Util

import Data.Monoid (mconcat)

readPosts :: IO [Post]
readPosts = map entityVal <$> (runDb $ selectList [PostDraft ==. False] [LimitTo 10])

main :: IO ()
main = do
  runDb $ runMigration migrateAll

  port <- liftM read $ getEnv "PORT"
  S.scotty port $ do
    S.get "/" $ do
      posts <- liftIO readPosts
      S.html $ T.pack $ show posts
