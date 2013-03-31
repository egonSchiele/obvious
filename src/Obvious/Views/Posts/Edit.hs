{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Edit where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import qualified Obvious.Views.Posts.Form
import Obvious.Model

render :: Num a => Post -> a -> Html
render post postId = Obvious.Views.Posts.Form.render (Just post) (Just postId) "/update"
