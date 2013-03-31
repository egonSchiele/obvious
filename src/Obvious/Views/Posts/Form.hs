{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Form where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import qualified Obvious.Views.Posts.Post
import Obvious.Model
import Obvious.Util

for_ = flip Prelude.map

split :: Num a => Maybe Post -> Maybe a -> Html
split post postId = do
    H.div ! A.id "split" $ do
      H.div ! A.id "post-editor" ! class_ "split-section" $ do
        H.div ! A.id "text-title" ! class_ "expandingArea" $ do
          pre $ do
            H.span ""
            br 
          textarea textareaContent ! rows "1" ! placeholder "Title here"
        fieldset ! class_ "markdown" $ do
          H.div "" ! A.id "text-content" ! class_ "expandingArea"
          pre $ do
            H.span ""
            br 
      H.div "" ! A.id "post-preview" ! class_ "split-section"
  where textareaContent = case post of
                            (Just post_) -> toHtml . postTitle $ post_
                            Nothing      -> "Put your content here"

render :: Show a => Num a => Maybe Post -> Maybe a -> String -> Html
render post postId postAction = do
    H.form ! method "post" ! action (toValue postAction) $ do
      split post postId
      publishBarHover post postId

publishBarHover :: Show a => Num a => Maybe Post -> Maybe a -> Html
publishBarHover post postId = do
    H.div ! A.id "publish-bar-hover" $ do
      H.div ! A.id "publish-bar" $ do
        H.div ! class_ "contain" $ do
          H.div ! class_ "left" $ do
            a "Admin" ! href "/admin"
            case postId of
              (Just postId_) -> a "Delete" ! href (toValue $ "/delete/" ++ (show postId_)) ! class_ "delete-bar"
              Nothing -> ""
            a "Options" ! href "#options" ! class_ "menu"
            ul ! A.id "options" ! class_ "sub-menu" $ do
              li $ do
                H.label "Url" ! for "url"
                input ! type_ "text" ! A.id "url" ! name "url" ! placeholder "Leave blank to automatically generate"

          H.div ! class_ "right" $ do
            H.label "Draft" ! for "draft"
            input ! type_ "checkbox" ! name "draft" ! A.id "draft"
            -- TODO preview is an arbitrary attribute, how to add?
            -- input ! type_ "submit" ! value "Preview" ! A.id "preview-button" ! preview "true" ! target "_blank"
            input ! type_ "submit" ! value "Save" ! A.id "save-button"
