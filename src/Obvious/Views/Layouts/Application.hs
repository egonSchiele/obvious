{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Layouts.Application where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Util
import qualified Web.Scotty as S
import qualified Data.Text.Lazy as TL
import Data.Monoid (mconcat)

-- config
-- required
blogTitle = "Obvious"
authorName = "Adit"

-- optional
tagLine = Just "Art for Nerds"
twitter = Just "_egonschiele"
github = Just "egonschiele"
email = Nothing
googleAnalyticsID = Nothing

appHeader = do
  h1 blogTitle
  ifJust tagLine (\tagLine_ -> H.span (toHtml tagLine_))
  ul $ do
    ifJust twitter $ \twitter_ -> do
      li $ do
        a (toHtml $ "@" ++ twitter_) ! href (toValue $ "http://twitter.com/" ++ twitter_)
    ifJust github $ \github_ -> do
      li $ do
        a "github" ! href (toValue $ "https://github.com/" ++ github_)
    ifJust email $ \email_ -> do
      li $ do
        a "say hi" ! href (toValue $ "mailto: " ++ email_ ++ "?subject:whats up!")
    li $ do
      a "rss feed" ! href "/posts.rss"
  H.span ! class_ "powered-by" $ do
    H.span "Powered by "
    a "Obvious" ! href "http://github.com/egonSchiele/obvious.git"
    iff isAdmin $ do
      a "Admin" ! href "/admin"
    

application :: Html -> Maybe TL.Text -> S.ActionM ()
application mainContent notice = blaze $ do
  html $ do
    H.head $ do
      H.title blogTitle
      meta ! name "author" ! content authorName
      link ! href "posts.rss" ! rel "alternate" ! A.title "RSS" ! type_ "application/rss+xml"
      link ! href "http://fonts.googleapis.com/css?family=Lato:300,900" ! rel "stylesheet" ! type_ "text/css"
      link ! href "/css/embed.css" ! rel "stylesheet" ! type_ "text/css"
      link ! href "/css/normalize.css" ! rel "stylesheet" ! type_ "text/css"
      link ! href "/css/posts.css" ! rel "stylesheet" ! type_ "text/css"
    H.body $ do
      header appHeader
      H.div ! A.id "container" $ do
        H.div ! A.id "content" $ do
          case notice of
            (Just notice_) -> H.span (toHtml notice_) ! class_ "notice"
            Nothing -> ""
          mainContent

      link ! type_ "text/javascript" ! src "/js/fitvids.js"
      link ! type_ "text/javascript" ! src "/js/jquery.js"
      link ! type_ "text/javascript" ! src "/js/posts.js"
      link ! type_ "text/javascript" ! src "/js/showdown.js"
      link ! type_ "text/javascript" ! src "/js/admin/_functions.js"
      link ! type_ "text/javascript" ! src "/js/admin/editor.js"
      link ! type_ "text/javascript" ! src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js"

    ifJust googleAnalyticsID $ \gaID -> do
      script . toHtml . mconcat $ [
        "var _gaq=[['_setAccount','" ++ gaID ++ "'],['_trackPageview']];",
        "(function(d,t){var g=d.createElement(t),s=d.getElementsByTagName(t)[0];",
        "g.src=('https:'==location.protocol?'//ssl':'//www')+'.google-analytics.com/ga.js';",
        "s.parentNode.insertBefore(g,s)}(document,'script'));"
        ]
