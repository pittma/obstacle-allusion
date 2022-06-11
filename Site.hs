{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import HakyllHacks

import System.FilePath

main :: IO ()
main =
  hakyllWithBaseRules $ do

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "blog/*"
        let context = listField
              "items"
              (defaultContext <> dateCtx <> blogRouteCtx <> teaser "posts") (return posts)
        getResourceBody >>= applyAsTemplate context

    match "blog/*" $ do
      route slugRoute
      compile $
        pandocCompiler >>=
        saveSnapshot "posts" >>=
        loadAndApplyTemplate "templates/post.html" (dateCtx <> defaultContext)

blogRouteCtx :: Context String
blogRouteCtx =
  field "blog-route" (return . dropFileName . dateSlug . itemIdentifier)

teaser :: String -> Context String
teaser = teaserField "teaser"
