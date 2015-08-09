--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
--
-- match :: Pattern -> Rules () -> Rules ()
-- route :: Routes -> Rules ()
-- compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules ()
-- compressCssCompiler :: Compiler (Item String)
-- pandocCompiler :: Compiler (Item String)
-- copyFileCompiler :: Compiler (Item CopyFile)
-- loadAndApplyTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
--  ex. loadAndApplyTemplate "templates/post.html" postCtx :: Item String -> Compiler (Item String)
--
-- loadAll :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
-- recentFirst :: MonadMetadata m => [Item a] -> m [Item a]

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- dateField :: String -> String -> Context a
-- defaultContext :: Context String
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
