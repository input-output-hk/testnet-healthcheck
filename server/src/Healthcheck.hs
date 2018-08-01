{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Healthcheck where

import Control.Lens (makeLenses, makePrisms)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , (.:)
  , defaultOptions
  , fieldLabelModifier
  , genericParseJSON
  , genericToJSON
  , parseJSON
  , toJSON
  , withObject
  )
import Data.List ()
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Servant ((:>), Get, JSON)
import Servant.Client
  ( BaseUrl
  , ClientM
  , ServantError
  , client
  , mkClientEnv
  , runClientM
  )

newtype HealthcheckResponse = HealthcheckResponse
  { _checks :: [Check]
  } deriving (Show, Eq, Generic)

instance ToJSON HealthcheckResponse where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

instance FromJSON HealthcheckResponse where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 1}

data Check
  = Ok Text
  | Error ErrorMessage
          Text
  deriving (Show, Eq, Generic)

instance FromJSON Check where
  parseJSON =
    withObject "check" $ \obj -> do
      status <- obj .: "status"
      description <- obj .: "description"
      case status of
        "OK" -> pure $ Ok description
        "ERROR" -> do
          err <- obj .: "error"
          pure $ Error err description
        _ -> fail $ "Unknown status: " <> status

instance ToJSON Check where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 1}

newtype ErrorMessage =
  ErrorMessage Text
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

makeLenses ''HealthcheckResponse

makePrisms ''Check

------------------------------------------------------------
type MantisAPI = "healthcheck" :> Get '[ JSON] HealthcheckResponse

getHealthcheck :: ClientM HealthcheckResponse
getHealthcheck = client (Proxy :: Proxy MantisAPI)

run :: MonadIO m => Manager -> BaseUrl -> ClientM a -> m (Either ServantError a)
run manager base action = liftIO $ runClientM action $ mkClientEnv manager base
