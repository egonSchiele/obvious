{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Index where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import qualified Obvious.Views.Posts.Post
import Obvious.Model

for_ = flip map

render :: Num a => [Post] -> [a] -> Html
render posts postIds = do
    for_ (zip posts postIds) $ \post, postId -> Obvious.Views.Posts.Post.render post postId False
