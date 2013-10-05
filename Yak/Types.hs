{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Yak.Types where

import Control.Applicative
import Control.Monad.Reader
import Data.Text (Text)
import Data.Yaml
import Text.Shakespeare.Text (st)

import qualified Data.Map as M
import qualified Data.Text as T

data FrontMatter = FrontMatter
    { fmLayout :: Maybe Text
    , fmTitle  :: Text
    , fmTags   :: [Text]
    } deriving Show

instance FromJSON FrontMatter where
    parseJSON (Object v) = FrontMatter
        <$> v .:? "layout"
        <*> v .: "title" .!= ""
        <*> v .: "tags"  .!= []

    parseJSON _ = mzero

data Layout = Layout
    { layoutLayout  :: Maybe Text
    , layoutContent :: Text
    }

data Post = Post
    { postLayout    :: Maybe Text
    , postTitle     :: Text
    , postSlug      :: Text
    , postPermalink :: Text
    , postTags      :: [Text]
    , postContent   :: Text
    }

data SiteConfig = SiteConfig
    { title    :: Text
    , url      :: Text
    , _layouts :: FilePath
    , _source  :: FilePath
    , _public  :: FilePath
    , _posts   :: FilePath
    }

data Site = Site
    { siteTitle   :: Text
    , siteUrl     :: Text
    , siteLayouts :: M.Map Text Layout
    , sitePosts   :: [Post]
    , siteSource  :: FilePath
    , siteOutput  :: FilePath
    }

type Yak a = ReaderT Site IO a

withSiteContext :: Text -> Yak Text
withSiteContext content = do
    site <- ask

    return $ [st|
    {
        "content": "#{escape content}",
        "site": {
            "title": "#{escape $ siteTitle site}",
            "url": "#{escape $ siteUrl site}",
            "posts": [#{escape $ T.intercalate ", " $ map postContext $ sitePosts site}]
        }
    }
    |]

withPostContext :: Text -> Post -> Text
withPostContext content p = [st|
{
    "content": "#{escape content}",
    "post": #{escape $ postContext p}
}
|]

postContext :: Post -> Text
postContext p = [st|
{
    "title": "#{escape $ postTitle p}",
    "permalink": "#{escape $ postPermalink p}",
    "content": "#{escape $ postContent p}"
    "tags": [#{escape $ T.intercalate ", " $ postTags p}],
}
|]

escape :: Text -> Text
escape = T.replace "\n" "\\\\n". T.replace "\"" "\\\""
