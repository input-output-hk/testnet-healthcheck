{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver
  ( run
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.BroadcastChan
  ( BroadcastChan
  , In
  , newBChanListener
  , newBroadcastChan
  , readBChan
  , writeBChan
  )
import Control.Lens (has, traversed)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (logErrorN, runStderrLoggingT)
import Data.Aeson (encode)
import Data.Default.Class (def)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Development.GitRev (gitHash)
import Healthcheck (HealthcheckResponse, _Error, checks, getHealthcheck)
import qualified Healthcheck
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.WebSockets (Connection, forkPingThread, sendTextData)
import Servant ((:<|>)((:<|>)), serve, serveDirectoryFileServer)
import Servant.API.WebSocket (WebSocket)
import Servant.Client (BaseUrl, ServantError)
import Servant.Server (Server)
import Webserver.API (Web)
import Webserver.Types (OverallStatus(Bad, Good, Unknown))

type Response = (UTCTime, Either ServantError HealthcheckResponse)

isOld :: UTCTime -> UTCTime -> Bool
isOld now test = diffUTCTime now test > maxAge
  where
    maxAge = 30

summarise :: UTCTime -> Response -> OverallStatus
summarise _ (_, Left _) = Unknown
summarise now (isOld now -> True, Right _) = Unknown
summarise _ (_, Right response) =
  if has (checks . traversed . _Error) response
    then Bad
    else Good

websocket :: BroadcastChan In Response -> Server WebSocket
websocket broadcaster = serveWebsocket
  where
    serveWebsocket :: MonadIO m => Connection -> m ()
    serveWebsocket connection =
      liftIO $ do
        chan <- newBChanListener broadcaster
        forkPingThread connection 10
        forever $ do
          response <- readBChan chan
          now <- getCurrentTime
          sendTextData connection $ encode $ summarise now response

version :: Applicative m => m Text
version = pure $(gitHash)

startWatcher :: BaseUrl -> IO (BroadcastChan In Response)
startWatcher healthcheckBaseUrl = do
  broadcaster <- newBroadcastChan
  manager <- newManager tlsManagerSettings
  _ <-
    forkIO $
    forever $ do
      now <- getCurrentTime
      health <- Healthcheck.run manager healthcheckBaseUrl getHealthcheck
      case health of
        Left err ->
          runStderrLoggingT $
          logErrorN $ "Error fetching status: " <> Text.pack (show err)
        _ -> pure ()
      writeBChan broadcaster (now, health)
      threadDelay (5 * 1000 * 1000)
  pure broadcaster

server :: BroadcastChan In Response -> FilePath -> Server Web
server broadcaster staticDir =
  version :<|> websocket broadcaster :<|> serveDirectoryFileServer staticDir

app :: BroadcastChan In Response -> FilePath -> Application
app broadcaster staticDir =
  middleware . serve (Proxy :: Proxy Web) $ server broadcaster staticDir

run :: MonadIO m => Settings -> BaseUrl -> FilePath -> m ()
run settings healthcheckBaseUrl staticDir =
  liftIO $ do
    broadcaster <- startWatcher healthcheckBaseUrl
    runSettings settings $ Webserver.app broadcaster staticDir

middleware :: Middleware
middleware = gzip def . logStdout . simpleCors
