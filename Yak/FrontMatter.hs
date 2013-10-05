{-# LANGUAGE OverloadedStrings #-}
module Yak.FrontMatter
    ( hasFrontMatter
    , splitFrontMatter
    ) where

import Control.Exception
import Data.Maybe
import Data.Text (Text)
import Data.Yaml
import System.IO
import Yak.Types

import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.IO as T

hasFrontMatter :: FilePath -> IO Bool
hasFrontMatter fp = do
    h    <- openFile fp ReadMode
    line <- fmap T.strip $ T.hGetLine h `catch` handleError

    return $ line == "---"

    where
        handleError :: IOException -> IO Text
        handleError _ = return ""

splitFrontMatter :: FilePath -> IO (FrontMatter, Text)
splitFrontMatter fp = do
    h    <- openFile fp ReadMode
    line <- T.hGetLine h

    frontMatter <-
        if line == "---"
            then readUntil (== "---") h
            else return ""

    content <- T.hGetContents h

    return (parseFrontMatter frontMatter, content)

readUntil :: (Text -> Bool) -> Handle -> IO Text
readUntil p h = go ""
    where
        go accum = do
            line <- T.hGetLine h

            if p line
                then return accum
                else do
                    next <- readUntil p h

                    return $ line `T.append` "\n" `T.append`  next

parseFrontMatter :: Text -> FrontMatter
parseFrontMatter = fromMaybe defaultFrontMatter . parseYaml

parseYaml :: FromJSON a => Text -> Maybe a
parseYaml = decode . C8.pack . T.unpack

defaultFrontMatter :: FrontMatter
defaultFrontMatter = FrontMatter
    { fmLayout = Nothing
    , fmTitle  = ""
    , fmTags   = []
    }
