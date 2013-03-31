{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Delete where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import qualified Obvious.Views.Posts.Post
import Obvious.Model

render :: Show a => Num a => Post -> a -> Html
render post postId = do
    h1 "Delete This Post?"
    H.form ! method "post" ! action "/destroy" $ do
      input ! type_ "hidden" ! A.id "id" ! name "id" ! value (toValue $ show postId)
      input ! type_ "submit" ! value "Yes"
    Obvious.Views.Posts.Post.render post postId False
