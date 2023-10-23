{-# LANGUAGE OverloadedStrings #-}

import Data.Binary (Binary)
import Data.List (intercalate, partition)
import Data.Typeable (Typeable)
import Hakyll

main :: IO ()
main = hakyll $ do

    tags <- makeTags

    matchSimple "images/**" copyFileCompiler
    matchSimple "css/*" compressCssCompiler
    matchSimple "webfonts/*" copyFileCompiler
    matchPages slushPattern (Just tags)
    matchPages postsPattern (Just tags)
    matchPages "notes/*" Nothing

    createPage "slush.html" slushPattern
    createPage "posts.html" postsPattern

    createPostsRSS
    createTags tags

    matchSimple "index.html"  indexCompiler
    matchSimple "pubkey.html" indexCompiler
    matchSimple "cardano.html" indexCompiler
    match "templates/*" $ compile templateBodyCompiler

matchSimple :: (Binary a, Typeable a, Writable a) => Pattern -> Compiler (Item a) -> Rules ()
matchSimple p c = match p  $ route idRoute >> compile c

tagCompiler :: String -> Pattern -> Compiler (Item String)
tagCompiler tag pat = do
    pages <- recentFirst =<< loadAll pat
    let (posts, slush) = partition (isPost . itemIdentifier) pages
        title = "tagged \"" ++ tag ++ "\""
        ctx = constField "title" title
               <> listField "posts" postCtx (pure posts)
               <> listField "slush" postCtx (pure slush)
               <> defaultContext

    makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
    where
       isPost :: Identifier -> Bool
       isPost iden = take 4 (toFilePath iden) == "post"

postsPattern :: Pattern
postsPattern = "posts/*"

slushPattern :: Pattern
slushPattern = "slush/*"

tagPattern :: Pattern
tagPattern = postsPattern .||. slushPattern

makeTags :: Rules Tags
makeTags = do
  let makeTagRule tag pat = route idRoute >> compile (tagCompiler tag pat)
  tags <- buildTags tagPattern (fromCapture "tags/*.html")
  tagsRules tags makeTagRule
  pure tags

matchPages :: Pattern -> Maybe Tags -> Rules ()
matchPages pat tags =
    match pat $ do
      let ctx = case tags of
                  Just ts -> postCtxWithTags ts
                  Nothing -> postCtx
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

createPage :: Identifier -> Pattern -> Rules ()
createPage name pat =
  create [name] $ do
    route idRoute
    compile $ do
      pages <- recentFirst =<< loadAll pat
      let ctx =
            listField "pages" postCtx (return pages)
              <> constField "title" " "
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

createPostsRSS :: Rules ()
createPostsRSS =
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots postsPattern "content"
      renderRss feedConfiguration feedCtx posts

createTags :: Tags -> Rules ()
createTags tags = create ["tags.html"] $ do
  route idRoute
  compile $ do
    let makeTagLink tag url _ _ _ = "<li><a href=\"" ++ url ++ "\">" ++ tag ++ "</a></li>"
        tagRenderer _ = renderTags makeTagLink (intercalate  "\n") tags
        tagsCtx =
          constField "title" "Tags"
            <> field "tags" tagRenderer
            <> defaultContext
    makeItem ""
      >>= loadAndApplyTemplate "templates/tags.html" tagsCtx
      >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
      >>= relativizeUrls

indexCompiler :: Compiler (Item String)
indexCompiler =
  getResourceBody
    >>= loadAndApplyTemplate "templates/default.html" defaultContext
    >>= relativizeUrls

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Jared Corduan"
    , feedDescription = "Personal blog of Jared Corduan"
    , feedAuthorName  = "Jared Corduan"
    , feedAuthorEmail = "jared.corduan@gmail.com"
    , feedRoot        = "https://jaredcorduan.github.io"
    }
