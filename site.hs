{-# LANGUAGE OverloadedStrings #-}
import           Data.List   (intercalate)
import           Data.Monoid (mappend)
import           Hakyll


main :: IO ()
main = hakyll $ do

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    tagsRules tags $ \tag pat -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pat
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "webfonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" " "            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- fmap (take 10) . recentFirst =<<
              loadAllSnapshots "posts/*" "content"
          renderRss feedConfiguration feedCtx posts

    create ["tags.html"] $ do
        route idRoute
        compile $ do
            let tagsCtx =
                    constField "title" "Tags"                `mappend`
                    field "tags" (\_ -> renderTags makeTagLink (intercalate  "\n") tags)  `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" tagsCtx
                >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $
            getResourceBody
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


makeTagLink :: String -> String -> Int -> Int -> Int -> String
makeTagLink  tag url _ _ _ = "<li><a href=\"" ++ url ++ "\">" ++ tag ++ "</a></li>"

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Jared Corduan"
    , feedDescription = "Personal blog of Jared Corduan"
    , feedAuthorName  = "Jared Corduan"
    , feedAuthorEmail = "jared.corduan@gmail.com"
    , feedRoot        = "https://jaredcorduan.github.io"
    }
