{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll
import HakyllHacks

import Control.Monad
import Data.Maybe
import System.FilePath
import Text.Pandoc.Options
import Text.Pandoc.SideNote (usingSideNotes)

main :: IO ()
main =
  hakyllWithBaseRules $ do

    tags <- buildTags "blog/*" (fromCapture "tags/*/index.html")

    tagsRules tags $ \tag pat -> do
      let title = "Posts tagged \"" ++ tag ++ "\""
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pat
        let context =
              constField "track" "true" <>
              constField "title" title <>
              listField "items" (tagsCtx tags) (return posts)
        makeItem "" >>= loadAndApplyTemplate "templates/tags.html" context

    match "tufte/tufte.css" $ do
      route idRoute
      compile copyFileCompiler

    match "blog/*" $ do
      route slugRoute
      compile $
        pandocWithSidenotes >>=
        loadAndApplyTemplate
          "templates/post.html"
          (dateCtx <> tagsField "tags" tags <> defaultContext)

    match "archive.html" $ do
      route toIdxPath
      compile $ do
        posts <- recentFirst =<< loadAll "blog/*"
        let context =
              defaultContext <>
              listField
                "items"
                (dateCtx <> blogRouteCtx <> defaultContext)
                (return posts)
        getResourceBody >>= applyAsTemplate context

    match "templates/*" $ compile templateCompiler

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- byHomeKey =<< loadAll "blog/*"
        let context =
              defaultContext <>
              listField
                "items"
                (dateCtx <> blogRouteCtx <> defaultContext)
                (return posts)
        getResourceBody >>= applyAsTemplate context
  
    match "*.html" $ do
      route toIdxPath
      compile $ asPostTemp defaultContext

byHomeKey :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a]
byHomeKey list = do
  homeKeyed <- foldM f [] list
  recentFirst homeKeyed
  where
    f ctx item = do
      let ident = itemIdentifier item
      md <- getMetadata ident
      if isJust $ lookupString "home" md
        then return $ item : ctx
        else return ctx
  

tagsCtx :: Tags -> Context String
tagsCtx tags = tagsField "tags" tags <> dateCtx <> blogRouteCtx <> defaultContext

asPostTemp :: Context String -> Compiler (Item String)
asPostTemp = asTempWithDefault "templates/post.html"

pandocWithSidenotes :: Compiler (Item String)
pandocWithSidenotes =
  let defWExt = writerExtensions defaultHakyllWriterOptions
      mathExtensions = [Ext_tex_math_dollars, Ext_latex_macros]
      extents = foldr enableExtension defWExt mathExtensions
      wopts =
        defaultHakyllWriterOptions
          {writerExtensions = extents, writerHTMLMathMethod = MathJax ""}
   in pandocCompilerWithTransform
        defaultHakyllReaderOptions
        wopts
        usingSideNotes

blogRouteCtx :: Context String
blogRouteCtx =
  field "blog-route" (return . dropFileName . dateSlug . itemIdentifier)

cleanRouteCtx :: Context String
cleanRouteCtx =
  field "clean-route" (return . clean . toFilePath . itemIdentifier)
  where
    clean path = takeDirectory path </> takeBaseName path
