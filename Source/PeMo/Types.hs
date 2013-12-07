module Types where

import Network.Xmpp
import qualified Data.Text as T

type Text = T.Text

data IMEvent = OnMessage Jid Text
             | OnPresence Jid Text -- PresenceType

data UIEvent = OnSend Jid Text

