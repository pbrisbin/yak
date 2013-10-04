module Yak.Liquid (liquid) where

import Data.Text (Text)
import System.Process (readProcess)

import qualified Data.Text as T

liquid :: Text -> Text -> IO Text
liquid template context = fmap T.pack
                        $ readProcess "liquid" [T.unpack context] (T.unpack template)
