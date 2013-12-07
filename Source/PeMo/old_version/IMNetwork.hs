module IMNetwork 
(
  login
, logout
, getBuddies
, sendIM
, getMsgElements
, elementType
, msgFrom
, getIMBody
 )

where

import Network.Xmpp hiding (Types)
import Network.Xmpp.IM
import Network.Socket hiding (isConnected)
import qualified Data.Text as T
import Data.XML.Types 
import Data.Default
import Data.Either
import Data.Maybe (fromJust)
import Data.Map (keys)
import Control.Monad (liftM)

type Connection = (Either XmppFailure Session)
type Text = T.Text

---- XML Parser ---------------
-- get elements of given message
getMsgElements :: Message -> [Element]
getMsgElements = messagePayload

-- returns type of message element
elementType :: Element -> String
elementType = T.unpack . nameLocalName . elementName

-- returns message 'from' username
msgFrom :: Message -> Maybe Text
msgFrom m = case (messageFrom m) of
              Nothing   -> Nothing
              Just jid  -> Just $ formatJid $ jidToText jid

-- returns IM body text
getIMBody :: Message -> Maybe Text
getIMBody m = case (getIM m) of
              Nothing -> Nothing
              Just im -> Just $ T.unlines (map (bodyContent) (imBody im))

formatJid :: Text -> Text
formatJid = T.takeWhile (/= '@')
-- ..................................- ---------------------


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
getBuddies :: Session -> IO [Jid]
getBuddies =  liftM f . getRoster
        where   f :: Roster -> [Jid]
                f r = [j | j <- keys (items r)]

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