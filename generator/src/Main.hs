--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
import           Data.Monoid (mappend)
import "process" System.Process ( callProcess )
import           Hakyll

--------------------------------------------------------------------------------

config :: Configuration
config =
    defaultConfiguration
        { destinationDirectory = "dist"
        , previewHost          = "127.0.0.1"
        , previewPort          = 8080
        , providerDirectory    = "src"
        , storeDirectory       = "generator/_cache"
        , tmpDirectory         = "generator/_tmp"
        }


main :: IO ()
main = hakyllWith config $ do
  copyImages
  processCssWithTailwind
  convertSupplementaryPagesMarkdownToHtml
  convertPostsMarkdownToHtml
  buildArchiveFromPostList
  convertIndexMarkdownToHtml
  compileTemplates


copyImages :: Rules ()
copyImages = match "images/*" $ do
  route   idRoute
  compile copyFileCompiler


processCssWithTailwind :: Rules ()
processCssWithTailwind = do
  -- Tailwind only compiles CSS rules which are actually used, so whenever we
  -- change the templates we have to completely recompile our CSS.
  templateDependency <- makePatternDependency "templates/*"
  rulesExtraDependencies [templateDependency] $
    create ["css/default.css"] $ do
      route   idRoute
      compile baseTailwindCssCompiler


convertSupplementaryPagesMarkdownToHtml :: Rules ()
convertSupplementaryPagesMarkdownToHtml =
  match (fromList ["about.md", "contact.md"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls


convertPostsMarkdownToHtml :: Rules ()
convertPostsMarkdownToHtml = match "posts/*" $ do
  route   $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/post.html"    postCtx
    >>= loadAndApplyTemplate "templates/default.html" postCtx
    >>= relativizeUrls


buildArchiveFromPostList :: Rules ()
buildArchiveFromPostList = create ["archive.html"] $ do
  route idRoute
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let archiveCtx = listField "posts" postCtx (return posts) <>
                      constField "title" "Archives" <>
                      defaultContext
    makeItem ""
      >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
      >>= loadAndApplyTemplate "templates/default.html" archiveCtx
      >>= relativizeUrls


convertIndexMarkdownToHtml :: Rules ()
convertIndexMarkdownToHtml = match "index.md" $ do
  route   $ setExtension "html"
  compile $ do
    posts <- recentFirst =<< loadAll "posts/*"
    let indexCtx = listField "posts" postCtx (return posts) <> defaultContext

    pandocCompiler
      >>= applyAsTemplate indexCtx
      >>= loadAndApplyTemplate "templates/default.html" indexCtx
      >>= relativizeUrls


compileTemplates :: Rules ()
compileTemplates = match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <> defaultContext


baseTailwindCssCompiler :: Compiler (Item String)
baseTailwindCssCompiler = do
  TmpFile tmp <- newTmpFile "tailwind.out.XXX"
  tailwindOutput <- unsafeCompiler $ do
    callTailwind tailwindBaseCss tmp tailwindConfig
    readFile tmp
  makeItem tailwindOutput
    where
      tailwindBaseCss :: FilePath
      tailwindBaseCss = "./src/css/base.tailwind.css"

      tailwindConfig :: FilePath
      tailwindConfig = "./src/tailwind.config.js"

      callTailwind :: FilePath -> FilePath -> FilePath -> IO ()
      callTailwind input output config = callProcess "tailwindcss"
        [ "--input", input
        , "--output", output
        , "--config", config
        , "--minify"
       ]