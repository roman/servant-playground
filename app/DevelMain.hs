{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module DevelMain where

import RIO
import App.Component (buildApp)
import App.Component.Logger (logComponentEvents)
import Control.Monad.Component.Development (ComponentEvent(..), runComponentDevel)


main :: IO ()
main = do
  tmpLogOptions <- logOptionsHandle stdout True
  withReloadableLogFunc tmpLogOptions $ \(logFunc, reloadLogOptions) ->
    runComponentDevel
      (logComponentEvents logFunc)
      "servant-playground"
      (buildApp logFunc reloadLogOptions)
      (\app -> traceIO "Application Started")
