module IMNetwork 
(
  login
, logout
 )

where

import Network.Xmpp
import Network.Xmpp.IM (simpleIM, getRoster, rosterRemove, rosterAdd)
import Network.Socket hiding (isConnected)
import Data.Text.Internal
import Data.Default
import Data.Either
import Data.Maybe (fromJust)
--import Control.Monad
--import System.Log.Logger    

type Connection = (Either XmppFailure Session)

-- 
tryConnection :: HostName -> Text -> Text -> IO Connection
tryConnection domain user pass = session domain
                                 (Just (\_ -> ( [scramSha1 user Nothing pass]), Nothing))
                                 def

-- check if connection 
isConnected :: (Either XmppFailure Session) -> Bool
isConnected (Right s) = True
isConnected _         = False

-- 
getConnError :: Connection -> Maybe [Char]
getConnError (Left e) = Just (show e)
getConnError _        = Nothing

setState :: Bool -> Session -> IO Bool
setState True s = sendPresence presenceOnline  s
setState _    s = sendPresence presenceOffline s


login :: HostName -> Text -> Text -> IO (Maybe Session, [Char])
login h u p = do
                conn <- tryConnection h u p
                if (isConnected conn)
                then do
                    setState True (right conn)  -- TODO in case it returns False
                    return (Just (right conn), "")
                else
                    return (Nothing, fromJust (getConnError conn))


-- send message 
sendIM :: Session -> Jid -> Text -> IO Bool
sendIM s j t = sendMessage (simpleIM j t) s

-- send typing state
--sendTypingSt :: Session -> 

-- retrieve online buddies
--getBuddies :: Session -> [Char]


logout :: Session -> IO ()
logout s = do
              setState False s    -- TODO in case it returns False
              endSession s
            
right :: Either a b -> b
right (Right b) = b

-- (sess', error) <- login "yabasta.com" "zydeon" "e22a8e90"
-- (sess', error) <- login "jabber.se" "zydeon" "olecas"
-- let sess = fromJust sess'
-- sendIM sess (parseJid "mozhan@yabasta.com") "test"
-- rosterAdd (parseJid "mozhan@yabasta.com") Nothing [] sess
-- getRoster sess
-- rosterRemove (parseJid "mozhan") sess
-- sendMessage (Message {messageID = Nothing, messageFrom = jid, messageTo = Just (parseJid "mozhan@jabber.se"), messageLangTag = Nothing, messageType = Chat, messagePayload = [Element {elementName = Name {nameLocalName = "composing", nameNamespace = Nothing, namePrefix = Nothing}, elementAttributes = [], elementNodes = [NodeContent (ContentText "send sth")]}]}) sess