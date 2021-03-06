{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, runStderrLoggingT)
import Data.Function ((&))
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Development.GitRev (gitHash)
import Network (HostName)
import qualified Network.Monitoring.Riemann.Client as Riemann
import qualified Network.Monitoring.Riemann.Event as Riemann
import qualified Network.Monitoring.Riemann.TCPClient as Riemann
import Network.Wai.Handler.Warp
  ( HostPreference
  , defaultSettings
  , setHost
  , setPort
  )
import Options.Applicative
  ( Parser
  , ReadM
  , argument
  , auto
  , customExecParser
  , disambiguate
  , help
  , helper
  , idm
  , info
  , infoOption
  , long
  , maybeReader
  , metavar
  , option
  , optional
  , prefs
  , short
  , showDefault
  , str
  , strOption
  , value
  )
import qualified Riemann
import Servant.Client (BaseUrl, parseBaseUrl)
import qualified System.Remote.Monitoring as EKG
import UnliftIO (MonadUnliftIO)
import qualified Webserver

data Command = RunWebserver
  { _host :: HostPreference
  , _port :: Int
  , _riemannHost :: HostName
  , _healthcheckBaseUrl :: BaseUrl
  , _ekgPort :: Maybe Int
  , _staticDir :: FilePath
  } deriving (Show, Eq)

versionOption :: Parser (a -> a)
versionOption =
  infoOption $(gitHash) (short 'v' <> long "version" <> help "Show the version")

commandParser :: Parser Command
commandParser = do
  _host <-
    strOption
      (short 'b' <> long "bind" <> help "Webserver bind address" <> showDefault <>
       value "localhost")
  _port <-
    option
      auto
      (short 'p' <> long "port" <> help "Webserver port number" <> showDefault <>
       value 8080)
  _riemannHost <-
    strOption
      (short 'r' <> long "riemann" <> help "Reimann host" <> showDefault <>
       value "localhost")
  _healthcheckBaseUrl <-
    option
      (maybeReader parseBaseUrl :: ReadM BaseUrl)
      (short 's' <> long "server" <> help "Backend API server to monitor")
  _ekgPort <-
    optional
      (option
         auto
         (short 'e' <> long "ekg-port" <> help "EKG monitor port number"))
  _staticDir <-
    argument str (metavar "STATIC_DIR" <> help "Static directory to serve up")
  pure RunWebserver {..}

runCommand :: (MonadUnliftIO m, MonadLogger m) => Command -> m ()
runCommand RunWebserver {..} = do
  _ :: Maybe EKG.Server <-
    traverse (liftIO . EKG.forkServer "localhost") _ekgPort
  logInfoN . Text.pack $ "Running on " <> show _host <> ":" <> show _port
  riemannClient <- liftIO $ Riemann.tcpClient _riemannHost 5555
  Riemann.sendEvent riemannClient $
    Riemann.ok Riemann.service & Riemann.version & Riemann.description "Startup"
  Webserver.run settings riemannClient _healthcheckBaseUrl _staticDir
  where
    settings = setHost _host . setPort _port $ defaultSettings

main :: IO ()
main = do
  command <-
    customExecParser
      (prefs disambiguate)
      (info (helper <*> versionOption <*> commandParser) idm)
  runStderrLoggingT $ do
    logInfoN $ "Running: " <> Text.pack (show command)
    runCommand command
