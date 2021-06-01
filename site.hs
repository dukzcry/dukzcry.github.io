--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Control.Applicative
import           Control.Monad (liftM)
import           Data.Time.Format
import Hakyll.Images        ( loadImage
                            , ensureFitCompiler
                            , compressJpgCompiler
                            )
import System.FilePath
import Data.List
import Algorithms.NaturalSort
import qualified GHC.IO.Encoding as E
import           System.Exit
import System.Directory
import System.Process
--import Debug.Trace (traceShowId)

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
          ctx' <- makeGalleryCtx
          let ctx = ctx' `mappend` videoField
          getResourceString
            >>= renderPandoc
            >>= applyAsTemplate ctx
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/comment.html" (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    match galleryLossyImages $ do
        route   idRoute
        compile $ loadImage
            >>= ensureFitCompiler 1280 1024
            >>= compressJpgCompiler 90
    match galleryFiles $ do
        route   idRoute
        compile copyFileCompiler
    match galleryImages $ version "thumbnail" $ do
      route . customRoute $ (\x -> replaceExtension x (".thumb" ++ takeExtension x)) . toFilePath
      compile $ loadImage
        >>= ensureFitCompiler 300 128
    match (fromGlob $ folder ++ "/*/*.md") $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ pandocCompiler
            >>= relativizeUrls
    galleryDependencies <- makePatternDependency galleryFiles
    rulesExtraDependencies [galleryDependencies] $ do
      match (fromGlob $ folder ++ "/index.html") $ do
        route $ setExtension "html"
        compile $ do
          ctx <- makeGalleryCtx
          getResourceString
            >>= renderPandoc
            >>= applyAsTemplate ctx
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" (constField "title" "Галерея" `mappend` siteCtx)
            >>= relativizeUrls
    rulesExtraDependencies [galleryDependencies] $ do
      match galleryFiles $ version "page" $ do
        route . customRoute $ (<.> "html") . toFilePath
        compile $ do
          path <- toFilePath <$> getUnderlying
          galleryUnboxed <- gallery
          let [(_,ctx)] = filter (\ (x,_) -> x `equalFilePath` takeDirectory path) galleryUnboxed
          let [item] = filter (\ x -> elem' x `equalFilePath` path) ctx
          let prevElm = maybe missingField (\ x -> ctxMaker "prev" (\ _ -> x)) $ prev' item
          let nextElm = maybe missingField (\ x -> ctxMaker "next" (\ _ -> x)) $ next' item
          let ctx' = ctxMaker "" (\ _ -> item) `mappend`
                prevElm `mappend`
                nextElm `mappend`
                constField "baseurl" ("/" ++ folder) `mappend`
                (field "body" . return . loadBody . fromFilePath $ path <.> "md") `mappend`
                siteCtx
          makeItem ""
            >>= loadAndApplyTemplate "templates/gallery.html" (ctx' `mappend` postCtx)
            >>= relativizeUrls

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
    constField "host" "http://dukzcry.github.io" `mappend`
    siteCtx

siteCtx :: Context String
siteCtx = 
  activeClassField `mappend`
  constField "sitename" "ГЕРО.ИН" `mappend`
  defaultContext
-- https://groups.google.com/forum/#!searchin/hakyll/if$20class/hakyll/WGDYRa3Xg-w/nMJZ4KT8OZUJ 
activeClassField :: Context a 
activeClassField = functionField "activeClass" $ \[p] _ -> do 
  path <- toFilePath <$> getUnderlying
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

folder = "gallery"
galleryLossyImages =
  fromGlob (folder ++ "/*/*.jpg")
  .||. fromGlob (folder ++ "/*/*.jpeg")
galleryLosslessImages = fromGlob (folder ++ "/*/*.png")
galleryImages = galleryLossyImages .||. galleryLosslessImages
galleryVideos =
  fromGlob (folder ++ "/*/*.mp4")
galleryFiles = galleryImages .||. galleryVideos
-- https://www.reddit.com/r/haskell/comments/2nepr0/implementing_doubly_linked_lists_in_haskell/cmdadok?utm_source=share&utm_medium=web2x
data DList a = Empty | Cell { elem :: a, prev :: DList a, next :: DList a } deriving (Eq)
data GalleryItem a = GalleryItem { elem' :: String
                               , prev' :: Maybe (GalleryItem a)
                               , next' :: Maybe (GalleryItem a)
                               , url :: Compiler String
                               , page :: Compiler String
                               , thumbnail :: Compiler String
                               , video :: Bool
                               , previousPageNum :: Bool
                               , nextPageNum :: Bool
                               }
fromList :: [a] -> DList a
fromList = go Empty
    where go :: DList a -> [a] -> DList a
          go prev [] = Empty
          go prev (a:as) = head
              where head = Cell a prev tail
                    tail = go head as
toList :: DList a -> [a]
toList Empty = []
toList (Cell a _ next) = a : toList next
mapDList f = go
    where go Empty = Empty
          go item@Cell { Main.elem = e, prev = p, next = n } = Cell { Main.elem = f item, prev = go p, next = go n }
gallery = do
  recursiveContents <- unsafeCompiler $ getRecursiveContents (\_ -> return False) folder
  let contents = map (folder </>) recursiveContents

  -- last time modified
  contentsTime <- sequence $ map (getItemModificationTime . fromFilePath) contents
  let contentsWithTime = zip contentsTime contents
  let timeSortedList = sortBy (\ x y -> Prelude.compare (fst x) (fst y)) contentsWithTime
  --let (_,sortedContents) = unzip timeSortedList

  -- natural sorted
  let sortedContents = sortBy (\ x y -> Algorithms.NaturalSort.compare x y) contents

  let filteredContents = filter (matches galleryFiles . fromFilePath) sortedContents
  let groupContents = groupBy (\ x y -> takeDirectory x == takeDirectory y) filteredContents
  let linkedContents = map (\ l -> (takeDirectory $ head l, Main.fromList l)) groupContents
  let
    ctxMaker Cell { Main.elem = e, prev = p, next = n } =
      let versionUrl version path = fmap (maybe empty toUrl) . getRoute . setVersion version $ fromFilePath path
          prevElm = if p == Empty then empty else pure $ itemMaker (Main.elem p) p n empty empty
          nextElm = if n == Empty then empty else pure $ itemMaker (Main.elem n) p n empty empty
          itemMaker e p n prevElm nextElm = GalleryItem { elem' = e
                                                  , prev' = prevElm
                                                  , next' = nextElm
                                                  , url = versionUrl Nothing e
                                                  , page = versionUrl (Just "page") e
                                                  , thumbnail = versionUrl (Just "thumbnail") e
                                                  , video = matches galleryVideos $ fromFilePath e
                                                  , previousPageNum = p /= Empty
                                                  , nextPageNum = n /= Empty
                                                  }
      in itemMaker e p n prevElm nextElm
  return $ map (\ (dir,l) -> (dir, toList $ mapDList ctxMaker l)) linkedContents
ctxMaker prefix f =
  field (prefix ++ "url") (url . f) `mappend`
  field (prefix ++ "page") (page . f) `mappend`
  field (prefix ++ "thumbnail") (thumbnail . f) `mappend`
  boolField (prefix ++ "video") (video . f) `mappend`
  if prefix == "" then missingField else boolField "previousPageNum" (previousPageNum . f) `mappend`
  if prefix == "" then missingField else boolField "nextPageNum" (nextPageNum . f)
makeGalleryCtx = do
  galleryUnboxed <- gallery
  let listfieldMaker (folder,items) =
        let items' = map makeItem items
        -- variable names are fragile
        in listField (takeFileName folder) (ctxMaker "" itemBody) (sequence items') `mappend`
           listField (takeFileName folder ++ "preview") (ctxMaker "" itemBody) (sequence $ take 5 items')
  let ctx = map listfieldMaker galleryUnboxed
  return $ foldl1 mappend ctx `mappend` galleryField
galleryField = functionField "gallery" $ \[args] _ ->
  return $ unlines [
    "$for(" ++ args ++ ")$",
    "$if(video)$",
    "<a href=\"$page$\"><video width=\"128\" height=\"128\" preload=\"metadata\"><source src=\"$url$\"></video></a>",
    "$else$",
    "<a href=\"$page$\"><img src=\"$thumbnail$\"/></a>",
    "<link rel=\"prefetch\" href=\"$url$\">",
    "$endif$",
    "$endfor$"
  ]

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
