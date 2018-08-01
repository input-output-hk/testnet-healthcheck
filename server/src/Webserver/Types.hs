{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Webserver.Types
  ( OverallStatus(..)
  ) where

import Data.Aeson (ToJSON, defaultOptions, genericToJSON, object, toJSON)
import GHC.Generics (Generic)

data OverallStatus
  = Good
  | Bad
  | Unknown
  deriving (Show, Eq, Generic)

instance ToJSON OverallStatus where
  toJSON status = object [("status", genericToJSON defaultOptions status)]
