{-# LANGUAGE OverloadedStrings #-}

module Types where

import qualified Network.Xmpp as X
import qualified Data.Text as T

type Jid = X.Jid
type Text = T.Text

data UIAction = DisplayMsg Jid Text   -- display received messages
              | DisplayPrs Jid Text   -- display received presences

data IMAction = SendMsg Jid Text      -- sendMessage


