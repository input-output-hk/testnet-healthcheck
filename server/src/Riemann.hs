{-# LANGUAGE TemplateHaskell #-}

module Riemann
  ( service
  , version
  ) where

import Development.GitRev (gitHash)
import Network.Monitoring.Riemann.Event (Event, attribute, attributes)

service :: String
service = "testnet-healthcheck"

version :: Event -> Event
version = attributes [attribute "version" (Just $(gitHash))]
