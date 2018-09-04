{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

-- | hindent doesn't really play well with Servant's type operators, so
-- | just convenience we put the API spec in its own module, where we
-- | can avoid autoformatting.
module Webserver.API
  ( Web
  ) where

import Data.Text (Text)
import Servant (Raw, (:<|>), (:>), Get, PlainText)
import Servant.API.WebSocket (WebSocket)

type Web =
  "version" :> Get '[ PlainText] Text
  :<|>
  "ws" :> WebSocket
  :<|>
  Raw
