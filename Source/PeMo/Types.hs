{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Data.Text as T
import qualified Network.Xmpp as X


-- Channel actions
data UIAction = DisplayMsg Jid Text   -- display received messages
              | DisplayPrs Jid Text   -- display received presences
              | OpenConver Jid        -- open converstaion

data IMAction = SendMsg Jid Text      -- send message
              | Logout


type Jid = X.Jid
type Text = T.Text
type Session = X.Session
type LoginFailure = String
type Connection = (Either X.XmppFailure Session)

pack = T.pack
getJid' = X.getJid
