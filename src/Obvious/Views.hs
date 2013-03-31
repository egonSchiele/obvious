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
import Obvious.Views.Layouts.Application

for_ = flip Prelude.map

deletePost post postId = do
    H.form ! method "post" ! action "/destroy" $ do
      input ! value (toValue postId) ! type_ "hidden" ! name "id"
      p "Are you sure you want to delete this post?"
      input ! type_ "submit"
      renderPost post

editPost post postId = do
  h1 "Edit Post"
  H.form ! method "post" ! action "/update" $ do
    input ! value (toValue postId) ! type_ "hidden" ! name "id"
    H.label "Title" ! for "title"
    input ! value (toValue . postTitle $ post) ! type_ "text" ! A.id "title" ! name "title"
    H.label "Content" ! for "content"
    H.textarea (toHtml . postContent $ post) ! A.id "content" ! name "content"
    H.label "Draft?" ! for "draft"
    if (postDraft post)
      then input ! type_ "checkbox" ! A.id "draft" ! name "draft" ! checked "checked"
      else input ! type_ "checkbox" ! A.id "draft" ! name "draft"
    H.label "Aside?" ! for "aside"
    if (postAside post)
      then input ! type_ "checkbox" ! A.id "aside" ! name "aside" ! checked "checked"
      else input ! type_ "checkbox" ! A.id "aside" ! name "aside"    
    H.label "Url" ! for "url"
    input ! value (toValue . postUrl $ post) ! type_ "text" ! A.id "url" ! name "url"
    input ! type_ "submit"    

newPost = do
  h1 "New Post"
  H.form ! method "post" ! action "/create" $ do
    H.label "Title" ! for "title"
    input ! type_ "text" ! A.id "title" ! name "title"
    H.label "Content" ! for "content"
    H.textarea "Content here" ! A.id "content" ! name "content"
    H.label "Draft?" ! for "draft"
    input ! type_ "checkbox" ! A.id "draft" ! name "draft"
    H.label "Aside?" ! for "aside"
    input ! type_ "checkbox" ! A.id "aside" ! name "aside"
    H.label "Url" ! for "url"
    input ! type_ "text" ! A.id "url" ! name "url"
    input ! type_ "submit"
