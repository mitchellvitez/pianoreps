{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Util where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (toLower)
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration, PersistFieldSql(..))

-- due to template haskell ghc stage constraints, this needs to be in a separate module
dropPrefix :: Text -> Options
dropPrefix s = defaultOptions {fieldLabelModifier = map toLower . drop (T.length s)}

-- left, right, or hands together
data Hands = LH | RH | HT
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''Hands)

instance PersistField Hands where
  toPersistValue = PersistText . T.pack . show
  fromPersistValue (PersistText hands) = case hands of
    "LH" -> Right LH
    "RH" -> Right RH
    "HT" -> Right HT
    _ -> Left $ "Invalid hands encoding in database: " <> hands
  fromPersistValue v = Left $ "Invalid hands format in database: " <> T.pack (show v)

instance PersistFieldSql Hands where
  sqlType _ = SqlOther "hands"
