{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module App.Component.Database where

import RIO
import RIO.Orphans ()
import qualified RIO.Text as Text

import qualified System.Etc as Etc

import Control.Monad.Component (ComponentM, buildComponent, buildComponent_)

import Data.Pool (Pool, destroyAllResources)

import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Postgresql (SqlBackend, ConnectionString, createPostgresqlPool)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Data.Aeson ((.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (withObject, typeMismatch)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    Person
      name String
      age Int Maybe
      deriving Show
  |]

getConnString config =
    Etc.getConfigValueWith parseConnString ["database"] config
  where
    parseConnString = JSON.withObject "ConnString" $ \obj -> do
      user   <- obj .: "user"
      passwd <- obj .: "password"
      db     <- obj .: "database"
      port   <- obj .: "port"
      host   <- obj .: "host"
      return
        $ Text.encodeUtf8
        $ Text.unwords [ "user=" <> user
                       , "password=" <> passwd
                       , "host=" <> host
                       , "port=" <> tshow (port :: Int)
                       , "dbname=" <> db ]

getPoolSize config =
  Etc.getConfigValue ["database", "poolSize"] config

runMigrations :: HasCallStack => LogFunc -> Pool SqlBackend -> ComponentM ()
runMigrations logFunc pool = buildComponent_ "db migrations" $ do
  runRIO logFunc $
    runSqlPool (runMigration migrateAll) pool

buildDatabasePool :: LogFunc -> IORef Etc.Config -> ComponentM (Pool SqlBackend)
buildDatabasePool logFunc configRef = do
  config     <- readIORef configRef
  connString <- getConnString config
  poolSize   <- getPoolSize config

  pool <- buildComponent "databasePool"
    (runRIO logFunc (createPostgresqlPool connString poolSize))
    (destroyAllResources)

  runMigrations logFunc pool

  return pool
