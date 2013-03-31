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
import qualified Network.HTTP.Types as HTTP
import Data.Time (getCurrentTime)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Debug.Trace as D
import Database.Persist.Store
import Data.Int

-- views
import qualified Obvious.Views.Posts.Index
import qualified Obvious.Views.Posts.New
import qualified Obvious.Views.Posts.Show
import qualified Obvious.Views.Posts.Preview
import qualified Obvious.Views.Posts.Edit
import qualified Obvious.Views.Posts.Delete
import qualified Obvious.Views.Posts.Admin

import Obvious.Views.Layouts.Admin
import Obvious.Views.Layouts.Application

--readPosts :: IO [Post]
readPosts = (runDb $ selectList [PostDraft ==. False] [LimitTo 10])

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
      posts_ <- liftIO readPosts
      notice <- lookup "notice" <$> S.params      
      let posts = map entityVal posts_
      -- TODO fix
      -- let postIds = map (unKey . entityKey) posts_
      let postIds = [1..]
      application (Obvious.Views.Posts.Index.render posts postIds) notice

    S.get "/preview/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost (getKey postId)
      case post of
        (Just _post) -> application (Obvious.Views.Posts.Preview.render _post postId) Nothing
        Nothing -> S.status HTTP.status404

    S.get "/show/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost (getKey postId)
      case post of
        (Just _post) -> application (Obvious.Views.Posts.Show.render _post postId) Nothing
        Nothing -> S.status HTTP.status404

    S.get "/new" $ do
      application (Obvious.Views.Posts.New.render) Nothing

    S.post "/create" $ do
      now <- liftIO getCurrentTime
      title <- liftM TL.unpack $ S.param "title"
      content <- liftM TL.unpack $ S.param "content"
      draft <- isJust <$> lookup "draft" <$> S.params
      aside <- isJust <$> lookup "aside" <$> S.params
      url <- liftM TL.unpack $ S.param "url"
      liftIO $ runDb $ insert $ Post title (T.pack content) draft aside url Nothing now
      S.redirect "/notice='post created!'"

    S.post "/update" $ do
      postId <- liftM read $ S.param "id"
      title <- liftM TL.unpack $ S.param "title"
      content <- liftM TL.unpack $ S.param "content"
      draft <- isJust <$> lookup "draft" <$> S.params
      aside <- isJust <$> lookup "aside" <$> S.params
      url <- liftM TL.unpack $ S.param "url"
      liftIO $ runDb $ update (getKey postId) [PostTitle =. title, PostContent =. (T.pack content), PostDraft =. draft, PostAside =. aside, PostUrl =. url]
      S.redirect "/notice='post updated!'"

    S.get "/edit/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost (getKey postId)
      case post of
        (Just _post) -> application (Obvious.Views.Posts.Edit.render _post postId) Nothing
        Nothing -> S.status HTTP.status404

    S.get "/delete/:post" $ do
      postId <- liftM read $ S.param "post"
      post <- liftIO $ getPost (getKey postId)
      case post of
        (Just _post) -> application (Obvious.Views.Posts.Delete.render _post postId) Nothing
        Nothing -> S.status HTTP.status404      
      

    S.post "/destroy" $ do
      postId <- liftM read $ S.param "id"
      liftIO $ runDb $ delete (getKey postId)
      S.redirect "/notice='post deleted!'"      

    S.get "/admin" $ do
      posts_ <- liftIO readPosts
      let posts = map entityVal posts_
      -- TODO fix
      -- let postIds = map (unKey . entityKey) posts_      
      let postIds = [1..]
      admin (Obvious.Views.Posts.Admin.render posts postIds) Nothing
