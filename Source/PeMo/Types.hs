{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Network.Xmpp as X
import qualified Data.Text as T

type Text = T.Text
type Jid  = X.Jid

data IMEvent = OnMessage Jid Text
             | OnPresence Jid Text -- PresenceType

data UIEvent = OnSend Jid Text

