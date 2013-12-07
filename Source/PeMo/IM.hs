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
import Control.Concurrent
import Types

type LoginFailure = String
type Connection = (Either XmppFailure Session)
type Text = T.Text

instance Show InstantMessage where
    show im = "Subject: " ++ (T.unpack $ getIMSubject im) ++ "\nBody: " ++ (T.unpack $ getIMBody im)



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


imInit :: Chan IMEvent -> IO ()
imInit c = do 
            conn <- (login "jabber.se" "zydeon" "olecas")
            case conn of
                Left e  -> error e       -- login failed
                Right s -> imLoop c s

-- TODO: create parser to identify properly 'composing'/'paused'/'body' elements
imLoop :: Chan IMEvent -> Session -> IO ()
imLoop c s = forever $ do 
                msg <- getMessage s
                case (getIM msg) of
                    Nothing -> return ()
                    Just im -> do 
                            jid  <- return $ fromJust $ messageFrom msg
                            text <- return $ getIMBody im
                            if text == ""
                            then return ()
                            else writeChan c (OnMessage jid text)


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

getIMSubject :: InstantMessage -> Text
getIMSubject im = T.unlines (map (subjectContent) (imSubject im))


-- ..................................- ---------------------






right :: Either a b -> IO b
right (Right b) = return b



