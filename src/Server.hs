module Server where

import           App
import           Control.Monad.Cont       (lift)
import           Control.Monad.Logger     (logInfoNS, runStderrLoggingT)
import           Control.Monad.Reader     (runReaderT)
import           Data.Pool                (Pool)
import           Database.Persist.Sql     (SqlBackend)
import           Database.Persist.Sqlite  (createSqlitePool)
import           Methods                  (methods)
import           Network.HTTP.Types       (status200, status500)
import           Network.JsonRpc.Server   (call)
import           Network.Wai              (Request, Response, ResponseReceived,
                                           lazyRequestBody, responseLBS)
import           Network.Wai.Handler.Warp (run)

app = do
  pool <- runStderrLoggingT $ createSqlitePool (dbUri config) (dbPoolSize config)
  let env = Env {config = config, pool = pool}
  run (serverPort config) (requestHandler env)
  where
    config = defaultConfig

requestHandler :: Env -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
requestHandler env request respond =
  runStderrLoggingT $ do
    body <- lift $ lazyRequestBody request
    maybeResponse <- runReaderT (call methods body) env
    case maybeResponse of
      (Just response) -> lift . respond $ responseLBS status200 [("Content-Type", "application/json")] response
      Nothing -> lift . respond $ responseLBS status500 [("Content-Type", "application/json")] "Some error happend"
