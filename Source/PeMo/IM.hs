{-# LANGUAGE OverloadedStrings #-}

module IM where

import qualified Data.Text as T
import Data.Default
import Data.Either
import Data.Maybe (fromJust)
import Control.Monad (forever, void)
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Network.Xmpp hiding (Types, Jid, Session)
import Network.Xmpp.IM
import Network.Socket hiding (isConnected)
import Types


imInit :: Chan IMAction -> Chan UIAction -> Session -> IO ()
imInit cIM cUI s = do
                 forkIO $ listenThread s cIM
                 imLoop cUI s


-- Waits for an IM message and forwards its display action to UI
imLoop :: Chan UIAction -> Session -> IO ()
imLoop cUI s = forever $ do 
                       msg <- getMessage s
                       case (getIM msg) of
                           Nothing -> return ()
                           Just im -> do 
                                   jid  <- formatJid $ fromJust $ messageFrom msg
                                   text <- return $ getIMBody im
                                   if text == ""
                                       then return ()
                                       else writeChan cUI (DisplayMsg jid text)

-- Removes any extra information in jid related to the client
formatJid :: Jid -> IO Jid
formatJid j = return $ parseJid (takeWhile (/='/') $ T.unpack $ jidToText j)


-- Waits on IM channel for an action and executes it
listenThread :: Session -> Chan IMAction -> IO ()
listenThread s ch = forever $ do
                            action <- readChan ch
                            case action of 
                                (SendMsg jid text) -> void $ sendMessage (simpleIM jid text) s
                                Logout             -> logout s


login :: HostName -> Text -> Text -> IO (Either LoginFailure Session)
login host user pass = do
                conn <- tryConnection host user pass
                if (isConnected conn)
                  then do sess <- right conn
                          setOnState sess
                          return (Right sess)
                  else return (Left $ fromJust (getConnError conn))


logout :: Session -> IO ()
logout s = do 
         setState False s
         endSession s


tryConnection :: HostName -> Text -> Text -> IO Connection
tryConnection domain user pass = session domain
                                         (Just (\_ -> ( [scramSha1 user Nothing pass]), Nothing))
                                         def

-- Checks if connected 
isConnected :: Connection -> Bool
isConnected (Right s) = True
isConnected _         = False

-- Returns error of connection 
getConnError :: Connection -> Maybe String
getConnError (Left e) = Just (show "Connection Failure!")    --TODO: Parse error and print it    (show e)
getConnError _        = Nothing


-- Sets online state
setOnState :: Session -> IO Bool
setOnState = setState True


-- Sets offline state
setOffState :: Session -> IO Bool
setOffState = setState False


setState :: Bool -> Session -> IO Bool
setState True s = sendPresence presenceOnline  s
setState _    s = sendPresence presenceOffline s


-- returns IM body text
getIMBody :: InstantMessage -> Text
getIMBody im = T.unlines (map (bodyContent) (imBody im))


right :: Either a b -> IO b
right (Right b) = return b

