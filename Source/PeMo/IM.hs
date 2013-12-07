{-# LANGUAGE OverloadedStrings #-}

module IM where

import Network.Xmpp hiding (Types)
import Network.Xmpp.IM
import Network.Socket hiding (isConnected)
import qualified Data.Text as T
import Data.Default
import Data.Either
import Data.Maybe (fromJust)
import Control.Monad (forever)
import Control.Concurrent.Chan
import Types

type LoginFailure = String
type Connection = (Either XmppFailure Session)

tryConnection :: HostName -> Text -> Text -> IO Connection
tryConnection domain user pass = session domain
                                 (Just (\_ -> ( [scramSha1 user Nothing pass]), Nothing))
                                 def

-- check if connection 
isConnected :: Connection -> Bool
isConnected (Right s) = True
isConnected _         = False

-- 
getConnError :: Connection -> Maybe String
getConnError (Left e) = Just (show e)
getConnError _        = Nothing

setOnState :: Session -> IO Bool
setOnState = setState True

setOffState :: Session -> IO Bool
setOffState = setState False

setState :: Bool -> Session -> IO Bool
setState True s = sendPresence presenceOnline  s
setState _    s = sendPresence presenceOffline s


login :: HostName -> Text -> Text -> IO (Either LoginFailure Session)
login h u p = do
                conn <- tryConnection h u p
                if (isConnected conn)
                then do
                    sess <- right conn
                    setOnState sess   -- TODO if return is false
                    return (Right sess)
                else
                    return (Left $ fromJust (getConnError conn))


imInit :: Chan IMEvent -> Chan UIEvent -> IO ()
imInit cIM cUI = do 
            conn <- (login "jabber.se" "zydeon" "olecas")
            case conn of
                Left e  -> error e       -- login failed
                Right s -> imLoop cIM s

-- TODO: create parser to identify properly 'composing'/'paused'/'body' elements
imLoop :: Chan IMEvent -> Session -> IO ()
imLoop cIM s = forever $ do 
                msg <- getMessage s
                case (getIM msg) of
                    Nothing -> return ()
                    Just im -> do 
                            jid  <- return $ fromJust $ messageFrom msg
                            text <- return $ getIMBody im
                            if text == ""
                            then return ()
                            else writeChan cIM (OnMessage jid text)


---- XML Parser ---------------
-- get elements of given message
--getMsgElements :: Message -> IO [Element]
--getMsgElements = return messagePayload

---- returns type of message element
--elementType :: Element -> String
--elementType = T.unpack . nameLocalName . elementName

-- returns IM body text
getIMBody :: InstantMessage -> Text
getIMBody im = T.unlines (map (bodyContent) (imBody im))

-- ..................................- ---------------------






right :: Either a b -> IO b
right (Right b) = return b



