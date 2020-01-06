module Server where

import           App
import           Control.Monad.Cont
import           Control.Monad.Logger
import           Control.Monad.Reader
import qualified Data.Text                as T
import           Database.Persist.Sqlite
import           Methods
import           Network.HTTP.Types
import           Network.JsonRpc.Server
import           Network.Wai
import           Network.Wai.Handler.Warp

app =
  runStderrLoggingT $ do
    logInfoNS "ROOT" "Creating database pool"
    pool <- createSqlitePool (dbUri config) (dbPoolSize config)
    let env = Env {config = config, pool = pool}
    logInfoNS "ROOT" ("Running server on port " <> (T.pack . show $ serverPort config))
    lift $ run (serverPort config) (requestHandler env)
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
