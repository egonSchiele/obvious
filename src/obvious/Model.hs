{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Obvious.Model where
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Post
    title String
    content Text
    draft Bool default=True
    aside Bool default=False
    url String
    parentId Int Maybe
    createdAt UTCTime
    deriving Show
Session
    sessionId String
    data Text
    deriving Show
    createdAt UTCTime    
|]
