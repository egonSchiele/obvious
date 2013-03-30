{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, EmptyDataDecls #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Main where
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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as Err
import qualified Data.ByteString as BS
import System.Environment
import Obvious.Model
import Obvious.Util
import Obvious.Views
import qualified Network.HTTP.Types as HTTP

import Data.Monoid (mconcat)

readPosts :: IO [Post]
readPosts = map entityVal <$> (runDb $ selectList [PostDraft ==. False] [LimitTo 10])

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x  

getPost :: Key SqlPersist Post -> IO (Maybe Post)
getPost id = do
    maybePost <- (runDb $ selectFirst [PostId ==. id] [])
    return $ maybePost `applyMaybe` (\xs -> Just $ entityVal xs)

main :: IO ()
main = do
  runDb $ runMigration migrateAll
  port <- liftM read $ getEnv "PORT"

  S.scotty port $ do
    S.get "/" $ do
      posts <- liftIO readPosts
      blaze $ renderPosts posts

    S.get "/preview/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost postId
      case post of
        (Just _post) -> blaze $ renderPost _post
        Nothing -> S.status HTTP.status404

    S.get "/show/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost postId
      case post of
        (Just _post) -> blaze $ renderPost _post
        Nothing -> S.status HTTP.status404

    S.get "/new" $ do
      blaze newPost

    S.post "/create" $ do
      S.html "TODO"

    S.get "/edit/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost postId
      case post of
        (Just _post) -> blaze $ editPost _post
        Nothing -> S.status HTTP.status404

    S.post "/destroy" $ do
      S.html "TODO"

    S.get "/admin" $ do
      S.html "TODO"


