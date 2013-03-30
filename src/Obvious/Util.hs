{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Obvious.Util where
import Database.Persist
import Database.Persist.GenericSql

-- uncomment this on dev
-- #define SQLite

#ifdef SQLite

import Database.Persist.Sqlite

runDb :: SqlPersist IO a -> IO a
runDb = withSqliteConn "dev.sqlite3" . runSqlConn

#else

import           Data.Monoid                 ((<>))
import           Data.Text.Encoding          (encodeUtf8)
import           Database.Persist.Postgresql (withPostgresqlConn)
import           Database.Persist.Store      (applyEnv, loadConfig)
import           Web.Heroku                  (dbConnParams)

runSql :: SqlPersist IO a -> IO a
runSql query = do
    params <- dbConnParams
    let connStr = Prelude.foldr (\(k,v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " "))
                  "" params
    withPostgresqlConn connStr $ runSqlConn query

#endif
