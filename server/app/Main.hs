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

data Command =
  Run HostPreference
      Int
      BaseUrl
      FilePath
  deriving (Show, Eq)

versionOption :: Parser (a -> a)
versionOption =
  infoOption $(gitHash) (short 'v' <> long "version" <> help "Show the version")

commandParser :: Parser Command
commandParser =
  Run <$>
  (strOption
     (short 'b' <> long "bind" <> help "Webserver bind address" <> showDefault <>
      value "127.0.0.1")) <*>
  option
    auto
    (short 'p' <> long "port" <> help "Webserver port number" <> showDefault <>
     value 8080) <*>
  option
    (maybeReader parseBaseUrl :: ReadM BaseUrl)
    (short 's' <> long "server" <> help "Backend API server to monitor") <*>
  argument str (metavar "STATIC_DIR" <> help "Static directory to serve up")

runCommand :: (MonadIO m, MonadLogger m) => Command -> m ()
runCommand (Run host port healthcheckBaseUrl staticDir) = do
  logInfoN . Text.pack $ "Running on " <> show host <> ":" <> show port
  Webserver.run settings healthcheckBaseUrl staticDir
  where
    settings = setHost host . setPort port $ defaultSettings

main :: IO ()
main = do
  command <-
    customExecParser
      (prefs disambiguate)
      (info (helper <*> versionOption <*> commandParser) idm)
  runStderrLoggingT $ runCommand command
