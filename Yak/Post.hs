{-# LANGUAGE QuasiQuotes #-}
module Yak.Post where

import Control.Monad.Reader
import Data.Text (Text)
import System.Directory
import System.FilePath.Posix
import Text.Shakespeare.Text (st)
import Yak.FrontMatter
import Yak.Layout
import Yak.Liquid
import Yak.Pandoc
import Yak.Types

import qualified Data.Text as T
import qualified Data.Text.IO as T

loadPosts :: FilePath -> IO [Post]
loadPosts _posts = do
    exists <- doesDirectoryExist _posts

    if exists
        then do
            postFiles <- getDirectoryContents _posts
            mapM (toPost _posts) $ filter isPost postFiles

        else return []

    where
        toPost :: FilePath -> FilePath -> IO Post
        toPost _posts fp = do
            let path = _posts </> fp

            (fm, content) <- splitFrontMatter path

            html <- pandoc content

            let slug = T.pack $ dropExtension fp

            return Post
                { postLayout    = fmLayout fm
                , postTitle     = fmTitle fm
                , postSlug      = slug
                , postPermalink = [st|/posts/#{slug}/|]
                , postTags      = fmTags fm
                , postContent   = html
                }

        isPost :: FilePath -> Bool
        isPost = (== ".md") . takeExtension

renderPost :: Post -> Yak ()
renderPost p = do
    site <- ask

    let path = joinPath 
            [ siteOutput site
            , "posts"
            , T.unpack $ postSlug p
            , "index.html"
            ]

    mlayout <- findLayout $ postLayout p

    rendered <- case mlayout of
        Just layout -> renderPostWithLayout p layout
        Nothing     -> renderPostWithoutLayout p

    liftIO $ T.writeFile path rendered

renderPostWithLayout :: Post -> Layout -> Yak Text
renderPostWithLayout p layout = do
    let context = withPostContext (postContent p) p

    rendered <- liftIO $ liquid (layoutContent layout) context
    mlayout  <- findLayout $ layoutLayout layout

    case mlayout of
        Just parent -> renderWithLayout rendered parent
        Nothing     -> return rendered

renderPostWithoutLayout :: Post -> Yak Text
renderPostWithoutLayout p = do
    let context = withPostContext (postContent p) p
    liftIO $ liquid (postContent p) context
