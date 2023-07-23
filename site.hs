--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Control.Applicative
import           Control.Monad (liftM)
import           Data.Time.Format
import Hakyll.Images (loadImage, ensureFitCompiler)
import Hakyll.Gallery
import System.FilePath
import qualified GHC.IO.Encoding as E
import           System.Exit
import System.Directory
import System.Process
--import Debug.Trace (traceShowId)

sitename = "Duck's Cry"
siteurl = "https://dukzcry.github.io"
sitedescription = "Утки, у-у-у!"

--------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllWith config $ do

    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    tagsRules tags $ \tag pattern -> do
        let title = "Посты на тему \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    pag <- buildPaginateWith grouper "posts/*" makeId
    paginateRules pag $ \pageNum pattern -> do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAllSnapshots pattern "content"
          let paginateCtx = paginateContext pag pageNum
              ctx =
                  constField "title" (if (pageNum == 1)
                  then "Главная"
                  else "Страница " ++ show pageNum) `mappend`
                  listField "posts" postCtx (return posts) `mappend`
                  paginateCtx `mappend`
                  siteCtx
          makeItem ""
              >>= loadAndApplyTemplate "templates/post-list.html" ctx
              >>= loadAndApplyTemplate "templates/default.html" ctx
              >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Архив"               `mappend`
                    tagCloudField "tag-cloud" 80 125 tags    `mappend`
                    siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["sitemap.xml"] $ do
         route   idRoute
         compile $ do
           posts <- recentFirst =<< loadAll "posts/*"
           pages <- loadAll "pages/*"
           let allPosts = (return (pages ++ posts))
           let sitemapCtx =
                    listField "entries" sitemapCtx' allPosts `mappend`
                    siteCtx
           makeItem ""
            >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    createFeed "rss.xml" renderRss

    createFeed "atom.xml" renderAtom

    create ["robots.txt"] $ do
      route idRoute
      compile $ do
        makeItem ""
          >>= loadAndApplyTemplate "templates/robots.txt" postCtx

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "content/*" $ do
        route   idRoute
        compile copyFileCompiler

    match ("content/*.jpg" .||. "content/*.jpeg" .||. "content/*.png") $ version "thumbnail" $ do
      route . customRoute $ (\x -> replaceExtension x (".thumb" ++ takeExtension x)) . toFilePath
      compile $ loadImage
        >>= ensureFitCompiler 300 128

    -- Compress CSS into one file.
    match "css/*" $ compile compressCssCompiler
    create ["style.css"] $ do
        route idRoute
        compile $ do
            csses <- loadAll "css/*.css"
            makeItem $ unlines $ map itemBody csses

    match "pages/*" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ do
          -- here we get listField for all gallery items
          ctx' <- makeGalleryCtx defaultGallerySettings
          let ctx = ctx' `mappend` videoField
          getResourceString
            >>= renderPandoc
            >>= galleryApplyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/comment.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    galleryRuleset defaultGallerySettings {
      --compressImages = (compressImages defaultGallerySettings) { compress = True },
      siteContext = siteCtx,
      postContext = postCtx
    }
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateFieldWith (defaultTimeLocale {
      months=[
        ("января","янв"), ("февраля","фев"),
        ("марта","мрт"), ("апреля","апр"),
        ("мая","май"), ("июня","июн"),
        ("июля","июл"), ("августа","авг"),
        ("сентября","сен"), ("октября","окт"),
        ("ноября","нбр"), ("декабря","дек")
      ]
    }) "date" "%e %B %Y" `mappend`
    siteCtx

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = sitename
    , feedDescription = sitedescription
    , feedAuthorName  = "Artem Lukyanov"
    , feedAuthorEmail = "dukzcry@ya.ru"
    , feedRoot        = siteurl
    }

createFeed name renderingFunction = create [name] $ do
      route idRoute
      compile $ do
          let feedCtx = postCtx `mappend` bodyField "description"
          posts <- recentFirst =<<
              loadAllSnapshots "posts/*" "content"
          renderingFunction myFeedConfiguration feedCtx posts

siteCtx :: Context String
siteCtx = 
  activeClassField `mappend`
  constField "sitename" sitename `mappend`
  constField "sitedescription" sitedescription `mappend`
  constField "host" siteurl `mappend`
  newPathField "path" `mappend`
  defaultContext
-- windows workaround
newPathField = mapContext normalizePath . pathField
normalizePath :: String -> String
normalizePath = map f
    where f '\\' = '/'
          f x = x
-- https://groups.google.com/forum/#!searchin/hakyll/if$20class/hakyll/WGDYRa3Xg-w/nMJZ4KT8OZUJ 
activeClassField :: Context a 
activeClassField = functionField "activeClass" $ \[p] _ -> do 
  path <- normalizePath . toFilePath <$> getUnderlying
  return $ if path == p then "active" else path

sitemapCtx' =
  modificationTimeField "modified" "%F" `mappend`
  postCtx

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags =
  field "iftags" (findId tags) `mappend`
  tagsField "tags" tags `mappend`
  postCtx
findId tags _ = do
  id <- getUnderlying
  ids <- pure $ concatMap snd $ tagsMap tags
  if (null $ filter (==id) ids)
  then empty
  else pure $ error ""

makeId :: PageNumber -> Identifier
makeId n = fromFilePath $ if (n == 1) then "index.html" else show n ++ "/index.html"
grouper ids = (liftM (paginateEvery 10) . sortRecentFirst) ids

videoFunc [file,width] _ =
  return $ unlines [
    "<video controls width=\"" ++ width ++ "\">",
    "<source src=\"" ++ file ++ "\" preload=\"metadata\">",
    "</video>"
  ]
videoFunc [file] _ =
  return $ unlines [
    "<video controls>",
    "<source src=\"" ++ file ++ "\" preload=\"metadata\">",
    "</video>"
  ]
videoField = functionField "video" videoFunc

command path cmd = do
  result <- withCurrentDirectory path $ spawnCommand cmd
  waitForProcess result
gitCommand path = do
      command path "git add *"
      command path "git commit -m 'deploy'"
      command path "git push"
config :: Configuration
config = defaultConfiguration
    { deploySite = deploy
    }
  where
    deploy :: Configuration -> IO ExitCode
    deploy c = do
      gitCommand $ providerDirectory c
      gitCommand $ destinationDirectory c
      pure ExitSuccess
