{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Admin where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import Obvious.Model

for_ = flip map

render :: Num a => [Post] -> [a] -> Html
render posts postIds = do
    div ! A.id "drafts" ! class_ "col" $ do
      h1 $ do
        text "Drafts "
        a "New Draft" ! "/new" ! class_ "button"
        -- TODO implement this functionality
        H.form $ do
          input ! type_ "text" ! name "title" ! A.id "title" ! placeholder "Start typing your title here..."
        ul $ do
          for_ (zip posts postIds) $ \post, postId -> do
            iff (postDraft post) $ do
              postLi post postId
    div ! A.id "published" ! class_ "col" $ do
      h2 $ do
        text "Published "
        a "Home" ! href "/" ! class_ "button"
      ul $ do
        for_ (zip posts postIds) $ \post, postId -> do
          iff (not $ postDraft post) $ do
            postLi post postId
        
