{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Form where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import qualified Obvious.Views.Posts.Post
import Obvious.Model

for_ = flip map

split :: Num a => Maybe Post -> Maybe a -> Html
split post postId = do
    div ! A.id "split" $ do
      div ! A.id "post-editor" ! class_ "split-section" $ do
        div ! A.id "text-title" ! class_ "expandingArea" $ do
          pre $ do
            span ""
            br 
          textarea textareaContent ! rows 1 ! placeholder "Title here"
        fieldset ! class_ "markdown" $ do
          div ! A.id "text-content" ! class_ "expandingArea"
          pre $ do
            span ""
            br 
      div ! A.id "post-preview" ! class_ "split-section"
  where textareaContent = case post of
                            (Just post_) -> toHtml . postTitle $ post_
                            Nothing      -> toHtml "Put your content here"

render :: Num a => Maybe Post -> Maybe a -> String -> Html
render post postId action = do
    form ! method "post" ! action (toValue action) $ do
      split post postId
      publishBarHover post postId

publishBarHover :: Num a => Maybe Post -> Maybe a -> Html
publishBarHover post postId = do
    div ! A.id "publish-bar-hover" $ do
      div ! A.id "publish-bar" $ do
        div ! class_ "contain" $ do
          div ! class_ "left" $ do
            a "Admin" ! href "/admin"
            ifJust postId $ \postId_ do
              a "Delete" ! href ("/delete/" ++ (show postId_)) ! class_ "delete-bar"
            a "Options" ! href "#options" ! class_ "menu"
            ul ! A.id "options" ! class_ "sub-menu" $ do
              li $ do
                label "Url" ! for "url"
                input ! type_ "text" ! A.id "url" ! name "url" ! placeholder "Leave blank to automatically generate"

          div ! class_ "right" $ do
            label "Draft" ! for "draft"
            input ! type_ "checkbox" ! name "draft" ! A.id "draft"
            -- TODO preview is an arbitrary attribute, how to add?
            input "Preview" ! type_ "submit" ! A.id "preview-button" ! preview "true" ! target "_blank"
            input "Save" ! type_ "save" ! A.id "save-button"
