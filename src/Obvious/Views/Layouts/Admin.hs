{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Obvious.Views.Layouts.Admin where
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Obvious.Utils

admin :: Html -> (Maybe String) -> Html
admin content notice = do
    html $ do
      H.head $ do
        title "Admin"
        link ! href "http://fonts.googleapis.com/css?family=Lato:300,900" ! rel "stylesheet" ! type_ "text/css"
        link ! href "css/application.css" ! rel "stylesheet" ! type_ "text/css"
      body ! class_ "admin" $ do
        div ! A.id "admin" $ do
          div ! A.id "save"
          ifJust notice $ \notice_ -> span notice_ ! class_ "notice"
          content

        link ! type_ "text/javascript" ! src "js/application.js"
