{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Util

dbName = "pianoreps.db"

data Exercise = Exercise
  { exerciseBpm :: Int
  , exerciseName :: Text
  , exerciseAll12Keys :: Bool
  } deriving (Eq, Show)
$(deriveJSON (dropPrefix "exercise") ''Exercise)

instance FromRow Exercise where
  fromRow = Exercise <$> field <*> field <*> field

instance ToRow Exercise where
  toRow (Exercise bpm name all12keys) = toRow (bpm, name, all12keys)

type API =
  "list" :> Get '[JSON] [Exercise] :<|>
  "add" :> ReqBody '[JSON] Exercise :> Post '[JSON] Exercise

main :: IO ()
main = do
  migrate
  putStrLn "running"
  run 8080 app

migrate :: IO ()
migrate = withConn $ \conn ->
  execute_ conn "CREATE TABLE IF NOT EXISTS exercises (bpm INTEGER, name TEXT PRIMARY KEY, all12keys BOOLEAN)"

addExercise :: Exercise -> Handler Exercise
addExercise exercise = liftIO $ withConn $ \conn -> do
  execute conn "INSERT INTO exercises (bpm, name, all12keys) VALUES (?,?,?)" exercise
  pure exercise

withConn :: (Connection -> IO a) -> IO a
withConn action = do
  conn <- open dbName
  a <- action conn
  close conn
  pure a

exercises :: Handler [Exercise]
exercises = liftIO $ withConn $ \conn ->
  query_ conn "SELECT * FROM exercises"

app :: Application
app = serve (Proxy :: Proxy API) (exercises :<|> addExercise)
