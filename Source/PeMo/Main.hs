{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp
import Network.Socket
import Data.Text.Internal
import Data.Text
import Control.Monad
import Data.Default
import System.Log.Logger
import Data.Maybe
import IMNetwork
import Control.Concurrent (forkIO)
import Graphics.Vty.Widgets.All

presenceThread :: Session -> IO ()
presenceThread s = forever $ do 
            p <- waitForPresence (\p -> presenceType p == Subscribe) s
            putStrLn (show p)

messageThread :: Session -> IO ()
messageThread s = forever $ do 
            msg <- getMessage s
            print $ formatMessage msg

--handleMsg :: Message -> IO a
--handleMsg m = case m of 

inputThread :: Session -> IO Bool
inputThread s = forever $ do
            text <- getLine
            sendIM s (parseJid "mozhan@jabber.se") (pack text)

main = do
        (sess', error) <- login "jabber.se" "zydeon" "olecas"
        let sess = fromJust sess'
        --b <- getBuddies sess
        --print b
        
        -- create threads
        --presenceThread sess
        messageThread  sess
        --inputThread sess

        --e <- editWidget
        --ui <- centered e

        --fg <- newFocusGroup
        --addToFocusGroup fg e

        --c <- newCollection
        --addToCollection c ui fg

        --e `onActivate` \this ->
        --            getEditText this >>= (error . ("You entered: " ++) . unpack)

        --runUi c defaultContext



