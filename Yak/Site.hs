{-# LANGUAGE OverloadedStrings #-}
module Yak.Site
    ( withSite
    , forAllSiteFiles
    , forAllSitePosts
    , module Yak.Types
    ) where

import Control.Monad
import Control.Monad.Reader
import System.Directory
import System.FilePath.Posix
import Yak.Layout
import Yak.Post
import Yak.Types

import qualified Data.Map as M

withSite :: SiteConfig -> Yak a -> IO a
withSite config f = do
    layouts <- loadLayouts $ _layouts config
    posts   <- loadPosts   $ _posts   config

    runReaderT f Site
        { siteTitle   = title config
        , siteUrl     = url config
        , siteLayouts = layouts
        , sitePosts   = posts
        , siteSource  = _source config
        , siteOutput  = _public config
        }

forAllSiteFiles :: (FilePath -> Yak ()) -> Yak ()
forAllSiteFiles f = do
    site <- ask
    forAllFiles (siteSource site) f

forAllSitePosts :: (Post -> Yak ()) -> Yak ()
forAllSitePosts f = do
    site <- ask
    mapM_ f $ sitePosts site

forAllFiles :: FilePath -> (FilePath -> Yak ()) -> Yak ()
forAllFiles _source f = do
    names <- liftIO $ getDirectoryContents _source

    forM_ names $ \name -> do
        interesting <- isInteresting name

        when interesting $ do
            let path = _source </> name

            isDirectory <- liftIO $ doesDirectoryExist path

            if isDirectory
                then forAllFiles path f else f path

    where
        isInteresting :: FilePath -> Yak Bool
        isInteresting "."     = return False
        isInteresting ".."    = return False
        isInteresting ('_':_) = return False
        isInteresting fp      = fmap ((/= fp) . siteOutput) ask
