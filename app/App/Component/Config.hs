{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module App.Component.Config (buildConfig) where

import RIO
import qualified RIO.Text as Text

import Control.Monad.Component (ComponentM, buildComponent_)

import Data.FileEmbed (embedFile)
import qualified System.Etc as Etc

configBytes :: ByteString
configBytes =
  $(embedFile "./config/spec.yaml")

readConfigSpec :: Either SomeException (Etc.ConfigSpec ())
readConfigSpec = do
   configTxt <- mapLeft toException $ Text.decodeUtf8' configBytes
   Etc.parseConfigSpec configTxt

loadConfig :: Etc.ConfigSpec cmd -> IO (Etc.Config, Vector SomeException)
loadConfig configSpec = do
  let defConfig = Etc.resolveDefault configSpec
  envConfig <- Etc.resolveEnv configSpec
  (fileConfig, warnings) <- Etc.resolveFiles configSpec
  return $ (defConfig <> envConfig <> fileConfig, warnings)

reloadConfig :: Etc.ConfigSpec () -> IORef Etc.Config -> IO ()
reloadConfig configSpec configRef = do
  (newConfig, _warnings) <- loadConfig configSpec
  atomicModifyIORef' configRef (\_ -> (newConfig, ()))

buildConfig :: ComponentM ( IORef Etc.Config
                          , IO ()
                          , Vector SomeException
                          )
buildConfig = buildComponent_ "config" $ do
  case readConfigSpec of
    Left err ->
      throwIO err

    Right configSpec -> do
      (config, warnings) <- loadConfig configSpec
      configRef          <- newIORef config
      return (configRef, reloadConfig configSpec configRef, warnings)
