{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Shared where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.Monoid (mconcat)

paginatePosts posts = do
    p ("1..." ++ (show . length $ posts))

postLi post postId = do
    li ! A.id ("post-" ++ (show postId)) $ do
      h3 $ do
        a (toHtml . postTitle $ post) ! href ("/edit/" ++ (show postId))
        span ! class_ "links" $ do
          a "View", ! href ("/show/" ++ (show postId)) ! class_ "admin-view"
          a "x" ! href ("/delete/" ++ (show postId)) ! class_ "admin-delete"
