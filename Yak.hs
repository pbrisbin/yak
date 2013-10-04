{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import Yak.File
import Yak.Post
import Yak.Site

main :: IO ()
main = do
    -- TODO: read _config.yaml?
    let siteConfig = SiteConfig
            { title    = "pbrisbin dot com"
            , url      = "http://pbrisbin.com"
            , _source  = "./"
            , _public  = "./_public"
            , _layouts = "./_layouts"
            , _posts   = "./_posts"
            }

    withSite siteConfig $ do
        forAllSiteFiles renderFile
        forAllSitePosts renderPost
