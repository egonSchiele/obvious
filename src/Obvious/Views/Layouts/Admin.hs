{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Layouts.Admin where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Util
import qualified Web.Scotty as S

admin :: Html -> (Maybe String) -> S.ActionM ()
admin content notice = blaze $ do
    html $ do
      H.head $ do
        H.title "Admin"
        link ! href "http://fonts.googleapis.com/css?family=Lato:300,900" ! rel "stylesheet" ! type_ "text/css"
        link ! href "/css/embed.css" ! rel "stylesheet" ! type_ "text/css"
        link ! href "/css/normalize.css" ! rel "stylesheet" ! type_ "text/css"
        link ! href "/css/posts.css" ! rel "stylesheet" ! type_ "text/css"        
      body ! class_ "admin" $ do
        H.div ! A.id "admin" $ do
          H.div "" ! A.id "save"
          ifJust notice $ \notice_ -> H.span (toHtml notice_) ! class_ "notice"
          content

        link ! type_ "text/javascript" ! src "/js/fitvids.js"
        link ! type_ "text/javascript" ! src "/js/jquery.js"
        link ! type_ "text/javascript" ! src "/js/posts.js"
        link ! type_ "text/javascript" ! src "js/showdown.js"
        link ! type_ "text/javascript" ! src "/js/admin/_functions.js"
        link ! type_ "text/javascript" ! src "/js/admin/editor.js"
        link ! type_ "text/javascript" ! src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.10.2/jquery-ui.min.js"
        
