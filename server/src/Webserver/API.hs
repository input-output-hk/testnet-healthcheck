{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Webserver.API
  ( Web
  ) where

import Data.Text (Text)
import Servant ((:<|>), (:>), Get, PlainText, Raw)
import Servant.API.WebSocket (WebSocket)

type Web
   = "version" :> Get '[ PlainText] Text
     :<|> "ws" :> WebSocket
     :<|> Raw
