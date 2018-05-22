{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component where

import RIO

import qualified System.Etc as Etc

import Database.Persist.Postgresql (SqlBackend, ConnectionString, createPostgresqlPool)
import Data.Pool (Pool)
import Control.Monad.Component (ComponentM)

import App.Component.Config (buildConfig)
import App.Component.Logger (buildLogReloader)
import App.Component.Database (buildDatabasePool)
import App.Component.Server (buildServer)


data Application
  = Application
  {
    appLogFunc      :: !LogFunc
  , appConfig       :: !(IORef Etc.Config)
  , appReloadLogger :: !(IO ())
  , appDbPool       :: !(Pool SqlBackend)
  , appServerAsync  :: !(Async ())
  }

buildApp :: LogFunc -> (LogOptions -> IO ()) -> ComponentM Application
buildApp logFunc reloadLogOptions = do
  (configRef, reloadConfig, configWarnings) <- buildConfig
  reloadLogger <- buildLogReloader configRef reloadConfig reloadLogOptions
  runRIO logFunc $ do
    -- Print warnings from reading config
    unless (null configWarnings)
      $ mapM_ (logWarn . display) configWarnings

    -- Print configuration values at beginning of program
    logInfo "\n# Application Configuration\n"
    readIORef configRef
      >>= Etc.renderConfig
      >>= logInfo . displayShow

  Application <$> pure logFunc
              <*> pure configRef
              <*> pure reloadLogger
              <*> buildDatabasePool logFunc configRef
              <*> buildServer configRef
