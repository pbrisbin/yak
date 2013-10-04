{-# LANGUAGE QuasiQuotes #-}
module Yak.Post where

import Control.Monad.Reader
import Data.Text (Text)
import System.Directory
import System.FilePath.Posix
import Text.Shakespeare.Text (st)
import Yak.FrontMatter
import Yak.Layout
import Yak.Pandoc
import Yak.Types

import qualified Data.Text as T

loadPosts :: FilePath -> IO [Post]
loadPosts _posts = do
    exists <- doesDirectoryExist _posts

    if exists
        then do
            postFiles <- getDirectoryContents _posts
            mapM toPost $ filter isPost postFiles

        else return []

    where
        toPost :: FilePath -> IO Post
        toPost fp = do
            (fm, content) <- splitFrontMatter fp

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

        permalink :: FilePath -> Text
        permalink fp = [st|/posts/#{dropExtension fp}/|]

        isPost :: FilePath -> Bool
        isPost = undefined

renderPost :: Post -> Yak ()
renderPost p = do
    site <- ask

    let path = joinPath 
            [ siteOutput site
            , "posts"
            , T.unpack $ postSlug p
            , "index.html"
            ]

    undefined
