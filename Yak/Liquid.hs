module Yak.Liquid (liquid) where

import Data.Text (Text)

-- TODO: echo content | liquid context
liquid :: Text -> Text -> IO Text
liquid template context = return template
