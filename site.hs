{-# LANGUAGE OverloadedStrings #-}

import Data.List (intercalate, partition)
import Hakyll

main :: IO ()
main = hakyll $ do

    tags <- makeTags

    match "images/**"  $ route idRoute >> compile copyFileCompiler
    match "css/*"      $ route idRoute >> compile compressCssCompiler
    match "webfonts/*" $ route idRoute >> compile copyFileCompiler
    matchPages slushPattern tags
    matchPages postsPattern tags

    createPage "slush.html" slushPattern
    createPage "posts.html" postsPattern

    createPostsRSS
    createTags tags

    match "index.html"  $ route idRoute >> compile indexCompiler
    match "templates/*" $ compile templateBodyCompiler

isPost :: Identifier -> Bool
isPost iden =
  case toFilePath iden of
    p:o:s:t:_ -> [p, o, s, t] == "post"
    _ -> False

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

matchPages :: Pattern -> Tags -> Rules ()
matchPages pat tags =
    match pat $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
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
