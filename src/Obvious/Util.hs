{-# LANGUAGE OverloadedStrings #-}

module Obvious.Util where
import Database.Persist
import Database.Persist.GenericSql
import System.Environment
import System.IO.Unsafe
import Control.Exception
import Database.Persist.Sqlite
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (withPostgresqlConn)
import Database.Persist.Store (applyEnv, loadConfig)
import Web.Heroku (dbConnParams)


data Environment = DEVELOPMENT | PRODUCTION deriving (Show, Eq)

env :: Environment
env = case (envVar) of
        Left _ -> DEVELOPMENT
        Right "development" -> DEVELOPMENT
        Right "production" -> PRODUCTION
        Right _ -> DEVELOPMENT
  where envVar = unsafePerformIO $ try (getEnv "ENV") :: Either (IOError) (String)

runDb :: SqlPersist IO a -> IO a
runDb query = if env == DEVELOPMENT
  then
    withSqliteConn "dev.sqlite3" . runSqlConn $ query
  else do
    params <- dbConnParams
    let connStr = Prelude.foldr (\(k,v) t -> t <> (encodeUtf8 $ k <> "=" <> v <> " "))
                  "" params
    withPostgresqlConn connStr $ runSqlConn query
