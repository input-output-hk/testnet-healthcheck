{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Webserver
  ( run
  ) where

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
import Control.Monad.Logger (MonadLogger, logInfoN)
import Data.Aeson (encode)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Development.GitRev (gitHash)
import Healthcheck (HealthcheckResponse, _Error, checks, getHealthcheck)
import qualified Healthcheck
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.Monitoring.Riemann.Client as Riemann
import qualified Network.Monitoring.Riemann.Event as Riemann
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (Settings, runSettings)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (gzip)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.WebSockets (Connection, forkPingThread, sendTextData)
import qualified Riemann
import Servant ((:<|>)((:<|>)), serve, serveDirectoryFileServer)
import Servant.API.WebSocket (WebSocket)
import Servant.Client (BaseUrl, ServantError)
import Servant.Server (Server)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (forkIO, threadDelay)
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

startWatcher ::
     (MonadUnliftIO m, Riemann.Client m client, MonadLogger m)
  => BaseUrl
  -> client
  -> m (BroadcastChan In Response)
startWatcher healthcheckBaseUrl riemannClient = do
  broadcaster <- liftIO newBroadcastChan
  manager <- liftIO $ newManager tlsManagerSettings
  _ <-
    forkIO . forever $ do
      updateHealthcheck manager healthcheckBaseUrl riemannClient broadcaster
      liftIO $ threadDelay (5 * 1000 * 1000)
  pure broadcaster

updateHealthcheck ::
     (MonadIO m, MonadLogger m, Riemann.Client m client)
  => Manager
  -> BaseUrl
  -> client
  -> BroadcastChan In Response
  -> m ()
updateHealthcheck manager healthcheckBaseUrl riemannClient broadcaster = do
  now <- liftIO getCurrentTime
  health <- Healthcheck.run manager healthcheckBaseUrl getHealthcheck
  let event = toRiemannEvent health
  logEvent event
  Riemann.sendEvent riemannClient event
  liftIO $ writeBChan broadcaster (now, health)

toRiemannEvent :: Either ServantError HealthcheckResponse -> Riemann.Event
toRiemannEvent (Left err) =
  Riemann.failure Riemann.service & Riemann.description "Error fetching status" &
  Riemann.attributes [Riemann.attribute "ServantError" (Just (show err))]
toRiemannEvent (Right response) =
  Riemann.ok Riemann.service & Riemann.description "Fetched status." &
  Riemann.attributes [Riemann.attribute "Response" (Just (show response))]

logEvent :: MonadLogger m => Riemann.Event -> m ()
logEvent = logInfoN . Data.Text.Lazy.toStrict . decodeUtf8 . encode

server :: BroadcastChan In Response -> FilePath -> Server Web
server broadcaster staticDir =
  version :<|> websocket broadcaster :<|> serveDirectoryFileServer staticDir

app :: BroadcastChan In Response -> FilePath -> Application
app broadcaster staticDir =
  middleware . serve (Proxy :: Proxy Web) $ server broadcaster staticDir

run ::
     (MonadLogger m, MonadUnliftIO m, MonadIO m, Riemann.Client m client)
  => Settings
  -> client
  -> BaseUrl
  -> FilePath
  -> m ()
run settings riemannClient healthcheckBaseUrl staticDir = do
  broadcaster <- startWatcher healthcheckBaseUrl riemannClient
  liftIO . runSettings settings $ app broadcaster staticDir

middleware :: Middleware
middleware = gzip def . logStdout . simpleCors
