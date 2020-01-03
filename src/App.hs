module App where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (LoggingT)
import           Control.Monad.Reader
import           Data.Pool              (Pool, withResource)
import           Data.Text
import           Database.Persist.Sql

data Config =
  Config
    { dbUri  :: Text
    , serverPort :: Int
    , dbPoolSize :: Int
    }

defaultConfig :: Config
defaultConfig = Config "db.sqlite" 8080 1

data Env =
  Env
    { pool   :: Pool SqlBackend
    , config :: Config
    }

type Server = ReaderT Env (LoggingT IO)

class (MonadIO m) => MonadDB m where
  runDatabaseStatement :: ReaderT SqlBackend m a -> m a

class (Monad m) => MonadConfig m where
  get :: (Config -> a) -> m a

instance MonadDB Server where
  runDatabaseStatement statement = do
    dbPool <- asks pool
    withResource dbPool (runReaderT statement)
    
instance MonadConfig Server where
  get getter = do
    appConfig <- asks config
    pure $ getter appConfig