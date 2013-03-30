{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, EmptyDataDecls #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Main where
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Maybe (isJust)
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Time
import qualified Web.Scotty as S
import System.IO.Unsafe
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Encoding.Error as Err
import qualified Data.ByteString as BS
import System.Environment
import Obvious.Model
import Obvious.Util
import Obvious.Views
import qualified Network.HTTP.Types as HTTP
import Data.Time (getCurrentTime)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Debug.Trace as D
import Database.Persist.Store
import Data.Int

readPosts :: IO [Post]
readPosts = map entityVal <$> (runDb $ selectList [PostDraft ==. False] [LimitTo 10])

getKey :: Int64 -> Key SqlPersist Post
getKey postId = (Key (PersistInt64 postId))

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b  
applyMaybe Nothing f  = Nothing  
applyMaybe (Just x) f = f x  

getPost :: Key SqlPersist Post -> IO (Maybe Post)
getPost id = do
    maybePost <- (runDb $ selectFirst [PostId ==. id] [])
    return $ liftM entityVal maybePost

main :: IO ()
main = do
  runDb $ runMigration migrateAll
  port <- liftM read $ getEnv "PORT"

  S.scotty port $ do
    S.middleware logStdoutDev
    S.middleware $ staticPolicy (addBase "/static")
    S.get "/" $ do
      posts <- liftIO readPosts
      blaze $ renderPosts posts

    S.get "/preview/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost (getKey postId)
      case post of
        (Just _post) -> blaze $ renderPost _post
        Nothing -> S.status HTTP.status404

    S.get "/show/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost (getKey postId)
      case post of
        (Just _post) -> blaze $ renderPost _post
        Nothing -> S.status HTTP.status404

    S.get "/new" $ do
      blaze newPost

    S.post "/create" $ do
      now <- liftIO getCurrentTime
      title <- liftM TL.unpack $ S.param "title"
      content <- liftM TL.unpack $ S.param "content"
      draft <- isJust <$> lookup "draft" <$> S.params
      aside <- isJust <$> lookup "aside" <$> S.params
      url <- liftM TL.unpack $ S.param "url"
      liftIO $ runDb $ insert $ Post title (T.pack content) draft aside url Nothing now
      S.html "done!"

    S.post "/update" $ do
      postId <- liftM read $ S.param "id"      
      title <- liftM TL.unpack $ S.param "title"
      content <- liftM TL.unpack $ S.param "content"
      draft <- isJust <$> lookup "draft" <$> S.params
      aside <- isJust <$> lookup "aside" <$> S.params
      url <- liftM TL.unpack $ S.param "url"
      liftIO $ runDb $ update (getKey postId) [PostTitle =. title, PostContent =. (T.pack content), PostDraft =. draft, PostAside =. aside, PostUrl =. url]

    S.get "/edit/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost (getKey postId)
      case post of
        (Just _post) -> blaze $ editPost _post (show postId)
        Nothing -> S.status HTTP.status404

    S.post "/destroy" $ do
      S.html "TODO"

    S.get "/admin" $ do
      S.html "TODO"


