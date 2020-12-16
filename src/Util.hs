module Util where

import Data.Aeson
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T

-- due to template haskell ghc stage constraints, this needs to be in a separate module
dropPrefix :: Text -> Options
dropPrefix s = defaultOptions {fieldLabelModifier = map toLower . drop (T.length s)}
