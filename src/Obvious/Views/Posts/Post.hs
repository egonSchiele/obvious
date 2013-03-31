{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Post where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import Obvious.Model
import Obvious.Util

render :: Show a => Num a => Post -> a -> Bool -> Html
render post postId preview = do
      section ! A.id "post" $ do
        H.div ! class_ "post contain" $ do
          if external 
            then do
              h1 ! class_ "external" $ do
                a (toHtml $ postTitle post) ! href (toValue $ postUrl post)
            else do
              h1 $ do
                a (toHtml $ postTitle post) ! href (toValue $ postSlug post) ! class_ "permalink"
              fromMarkdown (show $ postContent post)
              iff (isAdmin && not preview) $ do
                ul ! class_ "actions" $ do
                  li $ do
                    a "Edit" ! href (toValue $ "/edit/" ++ (show postId))
        button "Back to blog" ! class_ "button space-top" ! href "/"
    where external = (postUrl post) /= ""
