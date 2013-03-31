{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Layouts.Application where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
  ifJust tagLine (\tagLine_ -> span tagLine_)
  ul $ do
    ifJust twitter $ \twitter_ -> do
      li do
        a ("@" ++ twitter_) ! href ("http://twitter.com/" ++ twitter_)
    ifJust github $ \github_ -> do
      li do
        a "github" ! href ("https://github.com/" ++ github_)
    ifJust email $ \email_ -> do
      li do
        a "say hi" ! href ("mailto: " ++ email_ ++ "?subject:whats up!")
    li do
      a "rss feed" ! href "/posts.rss"
  span ! class_ "powered-by" $ do
    text "Powered by "
    a "Obvious" ! href "http://github.com/egonSchiele/obvious.git"
    iff admin $ do
      a "Admin" ! href "/admin"
    

application :: Html -> Maybe String -> Html
application content notice = do
  html $ do
    H.head $ do
      title blogTitle
      meta ! name "author" ! content authorName
      link ! href "posts.rss" ! rel "alternate" ! title "RSS" ! type_ "application/rss+xml"
      link ! href "http://fonts.googleapis.com/css?family=Lato:300,900" ! rel "stylesheet" ! type_ "text/css"
      link ! href "css/application.css" ! rel "stylesheet" ! type_ "text/css"
    H.body $ do
      header appHeader
      div ! A.id "container" $ do
        div ! A.id "content" $ do
          ifJust notice $ \notice_ -> span notice_ ! class_ "notice"
          content

      link ! type_ "text/javascript" ! src "js/application.js"

    ifJust googleAnalyticsID $ \gaID -> do
      -- TODO: multiline strings + string interpolation
      script [str|
        var _gaq=[['_setAccount','#{gaID}'],['_trackPageview']];
        (function(d,t){var g=d.createElement(t),s=d.getElementsByTagName(t)[0];
        g.src=('https:'==location.protocol?'//ssl':'//www')+'.google-analytics.com/ga.js';
        s.parentNode.insertBefore(g,s)}(document,'script'));
      ]
