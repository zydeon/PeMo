module Main where

data UIEvent = OnMessage Jid Text
             | OnPresence Jid PresenceType

data UIEvent = OnSend Jid Text

main = 