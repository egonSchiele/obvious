{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Posts.Admin where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Views.Shared
import Obvious.Model
import Obvious.Util
import Data.Monoid (mconcat)

for_ = flip Prelude.map

render :: Num a => Show a => [Post] -> [a] -> Html
render posts postIds = do
    H.div ! A.id "drafts" ! class_ "col" $ do
      h1 $ do
        H.span "Drafts "
        a "New Draft" ! href "/new" ! class_ "button"
        -- TODO implement this functionality
        H.form $ do
          input ! type_ "text" ! name "title" ! A.id "title" ! placeholder "Start typing your title here..."
        ul $ do
          mconcat $ for_ (zip posts postIds) $ \(post,postId) -> do
            iff (postDraft post) $ do
              postLi post postId
    H.div ! A.id "published" ! class_ "col" $ do
      h2 $ do
        H.span "Published "
        a "Home" ! href "/" ! class_ "button"
      ul $ do
        mconcat $ for_ (zip posts postIds) $ \(post,postId) -> do
          iff (not $ postDraft post) $ do
            postLi post postId
        
