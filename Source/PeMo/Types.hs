{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Network.Xmpp as X
import qualified Data.Text as T

type Jid = X.Jid
type Text = T.Text

data UIAction = DisplayMsg Jid Text   -- display received messages
              | DisplayPrs Jid Text   -- display received presences
              | OpenConver Jid

data IMAction = SendMsg Jid Text      -- sendMessage
              | Logout


type Session = X.Session

type LoginFailure = String
type Connection = (Either X.XmppFailure Session)

pack = T.pack
