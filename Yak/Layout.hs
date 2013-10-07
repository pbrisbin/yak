{-# LANGUAGE QuasiQuotes #-}
module Yak.Layout
    ( loadLayouts
    , findLayout
    , renderWithLayout
    , renderWithoutLayout
    ) where

import Control.Monad.Reader
import Data.Text (Text)
import System.Directory
import System.FilePath.Posix
import Text.Shakespeare.Text (st)
import Yak.FrontMatter
import Yak.Types
import Yak.Liquid

import qualified Data.Map as M
import qualified Data.Text as T

loadLayouts :: FilePath -> IO (M.Map Text Layout)
loadLayouts _layouts = do
    exists <- doesDirectoryExist _layouts

    if exists
        then do
            layoutFiles <- getDirectoryContents _layouts
            return . M.fromList =<< toMapList _layouts layoutFiles

        else return M.empty

    where
        toMapList :: FilePath -> [FilePath] -> IO [(Text, Layout)]
        toMapList _layouts = mapM (toMapListItem _layouts) . filter (`notElem` [".", ".."])

        toMapListItem :: FilePath -> FilePath -> IO (Text, Layout)
        toMapListItem _layouts fp = do
            let path = _layouts </> fp

            layout <- readLayout path

            return (T.pack $ dropExtension fp, layout)

        readLayout :: FilePath -> IO Layout
        readLayout fp = do
            (frontMatter, content) <- splitFrontMatter fp

            return Layout
                { layoutLayout  = fmLayout frontMatter
                , layoutContent = content
                }

findLayout :: Maybe Text -> Yak (Maybe Layout)
findLayout Nothing     = return Nothing
findLayout (Just name) = fmap (M.lookup name . siteLayouts) ask

renderWithoutLayout :: Text -> Yak Text
renderWithoutLayout content = do
    context <- withSiteContext content
    liftIO $ liquid content context

renderWithLayout :: Text -> Layout -> Yak Text
renderWithLayout content layout = do
    context  <- withSiteContext content
    rendered <- liftIO $ liquid (layoutContent layout) context
    mlayout  <- findLayout $ layoutLayout layout

    case mlayout of
        Just parent -> renderWithLayout rendered parent
        Nothing     -> return rendered
