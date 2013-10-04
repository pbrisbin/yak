module Yak.Pandoc (pandoc) where

import Data.Text (Text)
import System.Process (readProcess)

import qualified Data.Text as T

pandoc :: Text -> IO Text
pandoc markdown = fmap T.pack
                $ readProcess "pandoc" ["--from=markdown", "--to=html"] (T.unpack markdown)
