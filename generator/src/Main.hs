--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
import           Data.Monoid (mappend)
import "filepath" System.FilePath ( (</>) )
import "temporary" System.IO.Temp ( withSystemTempDirectory )
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
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile tailwindCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.md" $ do
        route $ setExtension "html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            pandocCompiler
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

tailwindCssCompiler :: Compiler (Item String)
tailwindCssCompiler = do
    inFile <- getResourceFilePath
    tailwindOutput <- unsafeCompiler
      -- `withSystemTempDirectory` used here over `withSystemTempFile`
      -- because with the latter this parent process would be holding
      -- the file lock at the point tailwind tried to write to it.
      $ withSystemTempDirectory "generator.tailwind.XXX"
      $ \tmpDir -> do
        -- No need to create the file as tailwind handles that
        let outFile = tmpDir </> "tailwind-out.css"
        callTailwind inFile outFile tailwindConfig
        readFile outFile
    makeItem tailwindOutput
  where
    tailwindConfig :: FilePath
    tailwindConfig = "./src/tailwind.config.js"

    callTailwind :: FilePath -> FilePath -> FilePath -> IO ()
    callTailwind input output config = callProcess "tailwindcss"
      [ "--input", input
      , "--output", output
      , "--config", config
      , "--minify"
      ]