{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.Server where

import RIO

import Control.Monad.Component (ComponentM, buildComponent)

import Data.Aeson ((.:?))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON (withObject)

import qualified System.Etc as Etc
import Control.Monad.Component (ComponentM, buildComponent, buildComponent_)

import qualified Network.Wai.Handler.Warp as Warp

import Lib (app)

fetchServerSettings
  :: (Etc.IConfig config, MonadThrow m) =>
     config -> m Warp.Settings
fetchServerSettings config =
    Etc.getConfigValueWith settingsParser ["http"] config
  where
    hostParser input =
      Warp.setHost (fromString input)

    settingsParser = JSON.withObject "Warp.Settings" $ \object -> do
      setPort    <- fmap Warp.setPort <$> (object .:? "port")
      setHost    <- fmap hostParser <$> (object .:? "host")
      setTimeout <- fmap Warp.setTimeout <$> (object .:? "request_timeout_seconds")

      return $ foldl' (&) Warp.defaultSettings (catMaybes [setPort, setHost, setTimeout])

buildServer :: IORef Etc.Config -> ComponentM (Async ())
buildServer configRef = do
  config <- readIORef configRef
  settings <- fetchServerSettings config
  buildComponent "http-server" (async (Warp.runSettings settings app)) cancel
