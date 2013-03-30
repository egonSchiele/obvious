{-# LANGUAGE OverloadedStrings #-}
module Obvious.Views where
import Control.Monad (forM_)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty as S
import Obvious.Model
import Data.Monoid (mconcat)

blaze = S.html . renderHtml

for_ = flip Prelude.map

wrap content = blaze $ do
  html $ do
    H.head $ do
      H.title "My Blog"
    body $ do
      H.div ! A.id "content" $ do
        content

renderPost post = do
  h1 . toHtml $ postTitle post
  p . toHtml $ postContent post

renderPosts posts = do
  mconcat $ for_ posts $ renderPost

editPost post = renderPost post

newPost = do
  h1 "New Post"
  H.label "Title" ! for "title"
  input ! type_ "text"
