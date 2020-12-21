{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.Except
import Control.Monad.Logger (LoggingT(..), monadLoggerLog, toLogStr, fromLogStr)
import Control.Monad.Reader (MonadReader, ask, asks, local)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Functor.Identity
import Data.List (intercalate)
import Data.Pool
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple
import Database.Persist
import Database.Persist.Sqlite
  ( acquireSqlConn
  , createSqlitePool
  , runMigration
  , runSqlite
  , runSqlPool
  , withSqlConn
  , ConnectionPool
  , PersistFieldSql(..)
  , SqlBackend(..)
  , SqlPersistT
  )
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Say
import Servant
import System.Random (randomRIO)
import System.Random.Pick
import Util
import Yesod.Core

data Config = Config
  { configPool :: ConnectionPool
  , configPort :: Port
  , configDatabaseName :: Text
  }

share [mkPersist sqlSettings, mkMigrate "migrate"] [persistLowerCase|
Exercise
  name Text
  UniqueName name
  bpm Int
  hands Hands
  allKeys Bool
  deriving Show
|]
$(deriveJSON (dropPrefix "exercise") ''Exercise)

type API =
  "list" :> Get '[JSON] [Exercise] :<|>
  "add" :> ReqBody '[JSON] Exercise :> Post '[JSON] () :<|>
  "populate" :> Get '[JSON] Exercise

newtype AppT m a =
  AppT { runApp :: ReaderT Config (ExceptT ServerError m) a }
  deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError, MonadIO)

type App = AppT IO

main :: IO ()
main = do
  withConfig $ \Config{..} -> do
    application <- initialize Config{..}
    putStrLn $ "running on port " <> show configPort
    run configPort application

instance MonadLogger IO where
  monadLoggerLog loc src lvl msg = do
    say $ T.pack (show lvl) <> ": " <> (decodeUtf8 . fromLogStr $ toLogStr msg)

initialize :: Config -> IO Application
initialize Config{..} = do
  runSqlPool (runMigration migrate) configPool
  pure $ app Config{..}

withConfig :: (Config -> IO a) -> IO a
withConfig action = do
  let dbname = "pianoreps.db"
  !pool <- createSqlitePool dbname 1
  action Config
    { configPool = pool
    , configPort = 1810
    , configDatabaseName = dbname
    }

runDB ::
  (MonadReader Config m, MonadIO m) =>
  SqlPersistT IO b -> m b
runDB query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool

app :: Config -> Application
app config = serve (Proxy :: Proxy API) $
  convertApp config exercises :<|>
  (\ex -> convertApp config (addExercise ex)) :<|>
  convertApp config populate

convertApp :: Config -> App a -> Handler a
convertApp config app = do
  Handler $ runReaderT (runApp app) config

-- Handlers --

addExercise :: Exercise -> App ()
addExercise exercise =
  runDB $ insert_ exercise

populate :: App Exercise
populate = do
  rand :: Int <- liftIO $ randomRIO (1, 1000)
  bpm :: Int <- liftIO $ randomRIO (60, 180)
  hands <- liftIO $ pickOne [LH, RH, HT]
  all12 <- liftIO $ pickOne [True, False]
  let exercise = Exercise ("Exercise " <> T.pack (show rand)) bpm hands all12
  runDB $ insert_ exercise
  pure exercise

exercises :: App [Exercise]
exercises = runDB $ map entityVal <$> selectList [] []
