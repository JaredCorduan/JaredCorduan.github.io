{-# LANGUAGE OverloadedStrings #-}

import Data.Binary (Binary)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.Typeable (Typeable)
import Hakyll
import System.Process (callProcess)

main :: IO ()
main = hakyll $ do

    tags <- makeTags

    matchSimple "images/**" copyFileCompiler
    matchSimple "css/*" compressCssCompiler
    matchSimple "webfonts/*" copyFileCompiler
    matchPages postsPattern (Just tags)
    matchPages "notes/*" Nothing

    createPage "posts.html" postsPattern "Posts"

    createFeeds
    createTags tags

    matchSimple "index.html"  indexCompiler
    matchSimple "pubkey.html" indexCompiler
    match "templates/*" $ compile templateBodyCompiler

    -- Resume PDF from LaTeX
    match "resume/resume.tex" $ do
        route $ constRoute "resume.pdf"
        compile pdfLatexCompiler

matchSimple :: (Binary a, Typeable a, Writable a) => Pattern -> Compiler (Item a) -> Rules ()
matchSimple p c = match p  $ route idRoute >> compile c

tagCompiler :: String -> Pattern -> Compiler (Item String)
tagCompiler tag pat = do
    posts <- recentFirst =<< loadAll pat
    let title = "tagged \"" ++ tag ++ "\""
        ctx = constField "title" title
               <> listField "posts" postCtx (pure posts)
               <> defaultContext

    makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

postsPattern :: Pattern
postsPattern = "posts/*"

makeTags :: Rules Tags
makeTags = do
  let makeTagRule tag pat = route idRoute >> compile (tagCompiler tag pat)
  tags <- buildTags postsPattern (fromCapture "tags/*.html")
  tagsRules tags makeTagRule
  pure tags

matchPages :: Pattern -> Maybe Tags -> Rules ()
matchPages pat tags =
    match pat $ do
      let ctx = maybe postCtx postCtxWithTags tags
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

createPage :: Identifier -> Pattern -> String -> Rules ()
createPage name pat title =
  create [name] $ do
    route idRoute
    compile $ do
      pages <- recentFirst =<< loadAll pat
      let ctx =
            listField "pages" postCtx (return pages)
              <> constField "title" title
              <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

createFeeds :: Rules ()
createFeeds = do
  create ["rss.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots postsPattern "content"
      renderRss feedConfiguration feedCtx posts
  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx <> bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots postsPattern "content"
      renderAtom feedConfiguration feedCtx posts

createTags :: Tags -> Rules ()
createTags tags = create ["tags.html"] $ do
  route idRoute
  compile $ do
    let makeTagLink tag url count _ _ = "<li><a href=\"" ++ url ++ "\">" ++ tag ++ " (" ++ show count ++ ")</a></li>"
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

pdfLatexCompiler :: Compiler (Item LBS.ByteString)
pdfLatexCompiler = do
    texPath <- getResourceFilePath
    unsafeCompiler $ do
        callProcess "pdflatex"
            [ "-interaction=nonstopmode"
            , "-output-directory=resume"
            , texPath
            ]
        LBS.readFile "resume/resume.pdf"
    >>= makeItem
