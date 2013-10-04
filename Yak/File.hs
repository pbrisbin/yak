module Yak.File
    ( renderFile
    , ensureDirectory
    ) where

import Control.Monad.Reader
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory
import System.FilePath.Posix
import Yak.FrontMatter
import Yak.Layout
import Yak.Liquid
import Yak.Types

import qualified Data.Text.IO as T

renderFile :: FilePath -> Yak ()
renderFile fp = do
    renderable <- liftIO $ hasFrontMatter fp

    if renderable
        then do
            (fm, content) <- liftIO $ splitFrontMatter fp
            mlayout       <- findLayout $ fmLayout fm
            path          <- publicPath fp

            rendered <-
                case mlayout of
                    Just layout -> renderWithLayout content layout
                    Nothing     -> renderWithoutLayout content

            writeContent rendered path

        else copyContent fp

writeContent :: Text -> FilePath -> Yak ()
writeContent content fp = do
    path <- publicPath fp

    liftIO $ do
        ensureDirectory path
        T.writeFile path content

publicPath :: FilePath -> Yak FilePath
publicPath fp = do
    site <- ask

    let src = siteSource site
    let pub = siteOutput site

    return $ pub </> (fromMaybe fp $ stripPrefix src fp)

copyContent :: FilePath -> Yak ()
copyContent fp = do
    path <- publicPath fp
    liftIO $ do
        ensureDirectory path
        copyFile fp path

ensureDirectory :: FilePath -> IO ()
ensureDirectory fp = do
    let (dir,_) = splitFileName fp
    createDirectoryIfMissing True dir
