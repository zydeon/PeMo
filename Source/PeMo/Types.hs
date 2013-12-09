{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Network.Xmpp as X
import qualified Data.Text as T

type Jid = X.Jid
type Text = T.Text

data IMEvent = OnMessage Jid Text
             | OnPresence Jid Text -- PresenceType

data UIEvent = OnSend Jid Text


