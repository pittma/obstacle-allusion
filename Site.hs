{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import HakyllHacks

main :: IO ()
main =
  hakyllWithBaseRules $ do

    match "index.html" $ do
      route idRoute
      compile copyFileCompiler
