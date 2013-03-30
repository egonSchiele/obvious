{-# LANGUAGE OverloadedStrings #-}

module Obvious.Util where
import Database.Persist
import Database.Persist.Sqlite
import Data.Text 

runDb :: SqlPersist IO a -> IO a
runDb = withSqliteConn "dev.sqlite3" . runSqlConn
