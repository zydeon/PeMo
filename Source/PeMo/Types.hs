module Types where

import Network.Xmpp
import Data.Text

data IMEvent = OnMessage Jid Text
             | OnPresence Jid Text -- PresenceType

data UIEvent = OnSend Jid Text