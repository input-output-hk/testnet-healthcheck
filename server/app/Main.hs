{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.BroadcastChan
  ( BroadcastChan
  , In
  , newBChanListener
  , newBroadcastChan
  , readBChan
  , writeBChan
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, runStderrLoggingT)
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Development.GitRev (gitHash)
import Network.Wai.Handler.Warp
  ( HostPreference
  , defaultSettings
  , runSettings
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
  , prefs
  , short
  , showDefault
  , str
  , strOption
  , value
  )
import Servant.Client (BaseUrl(BaseUrl), parseBaseUrl)
import qualified Webserver

data Command = RunWebserver
  { _host :: HostPreference
  , _port :: Int
  , _healthcheckBaseUrl :: BaseUrl
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
       value "127.0.0.1")
  _port <-
    option
      auto
      (short 'p' <> long "port" <> help "Webserver port number" <> showDefault <>
       value 8080)
  _healthcheckBaseUrl <-
    option
      (maybeReader parseBaseUrl :: ReadM BaseUrl)
      (short 's' <> long "server" <> help "Backend API server to monitor")
  _staticDir <-
    argument str (metavar "STATIC_DIR" <> help "Static directory to serve up")
  pure RunWebserver {..}

runCommand :: (MonadIO m, MonadLogger m) => Command -> m ()
runCommand RunWebserver {..} = do
  logInfoN . Text.pack $ "Running on " <> show _host <> ":" <> show _port
  Webserver.run settings _healthcheckBaseUrl _staticDir
  where
    settings = setHost _host . setPort _port $ defaultSettings

main :: IO ()
main = do
  command <-
    customExecParser
      (prefs disambiguate)
      (info (helper <*> versionOption <*> commandParser) idm)
  runStderrLoggingT $ runCommand command
