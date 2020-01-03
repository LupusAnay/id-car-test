module Methods where

import           App
import           Control.Monad.Reader
import           Data
import           Database.Persist
import           Database.Persist.Sql
import           Network.JsonRpc.Server

methods :: [Method Server]
methods = [createProjectMethod, getAllProjectsMethod]

createProjectMethod = toMethod "createProject" createProject (Required "project" :+: ())

getAllProjectsMethod = toMethod "getAllProjects" getAllProjects ()

createProject :: (MonadConfig m, MonadDB m) => Project -> RpcResult m String
createProject project = do
  id <- lift . runDatabaseStatement $ insert project
  pure . show . fromSqlKey $ id

getAllProjects :: (MonadConfig m, MonadDB m) => RpcResult m [Entity Project]
getAllProjects = lift . runDatabaseStatement $ selectList [] [Asc ProjectName]
