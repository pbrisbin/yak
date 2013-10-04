module Yak.Pandoc (pandoc) where

import Data.Text (Text)

-- TODO: echo content | pandoc --from=markdown --to=html or use Pandoc
pandoc :: Text -> IO Text
pandoc markdown = return markdown
