{-# LANGUAGE OverloadedStrings #-}

module IM where

import Network.Xmpp hiding (Types, Jid)
import Network.Xmpp.IM
import Network.Socket hiding (isConnected)
import qualified Data.Text as T
import Data.Default
import Data.Either
import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Types

type LoginFailure = String
type Connection = (Either XmppFailure Session)


imInit :: Chan IMEvent -> Chan UIEvent -> Session -> IO ()
imInit cIM cUI s = do  
              forkIO $ listenThread s cUI
              imLoop cIM s

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

listenThread :: Session -> Chan UIEvent -> IO ()
listenThread s ch = forever $ do
        ev <- readChan ch
        case ev of 
            (OnSend jid text) -> void $ sendIM s jid text


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


-- send message 
sendIM :: Session -> Jid -> Text -> IO Bool
sendIM s j t = sendMessage (simpleIM j t) s

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

-- returns IM body text
getIMBody :: InstantMessage -> Text
getIMBody im = T.unlines (map (bodyContent) (imBody im))

-- ..................................- ---------------------


right :: Either a b -> IO b
right (Right b) = return b



