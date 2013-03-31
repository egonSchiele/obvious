{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Shared where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Model
import Obvious.Util

import Data.Monoid (mconcat)

paginatePosts posts = do
    p (toHtml $ "1..." ++ (show . length $ posts))

postLi post postId = do
    li ! A.id (toValue $ "post-" ++ (show postId)) $ do
      h3 $ do
        a (toHtml . postTitle $ post) ! href (toValue $ "/edit/" ++ (show postId))
        H.span ! class_ "links" $ do
          a "View" ! href (toValue $ "/show/" ++ (show postId)) ! class_ "admin-view"
          a "x" ! href (toValue $ "/delete/" ++ (show postId)) ! class_ "admin-delete"
