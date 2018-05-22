{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Import
import App.Component (buildApp)
import App.Component.Logger (logComponentEvents)
import Control.Monad.Component (ComponentEvent(..), runComponentM1)

main :: IO ()
main = do
  tmpLogOptions <- logOptionsHandle stdout False
  withReloadableLogFunc tmpLogOptions $ \(logFunc, reloadLogOptions) ->
    runComponentM1 (logComponentEvents logFunc)
                   "servant-playground"
                   (buildApp logFunc reloadLogOptions)
                   (\app -> traceIO "Application Started")
