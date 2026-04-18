{-# LANGUAGE CPP, OverloadedStrings #-}

module Main (main) where

import Data.Void

import Miso

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = startApp defaultEvents app

app :: App () Void
app = component () absurd $ const $ text "Hello, world!"
