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

presenceThread :: Session -> IO ()
presenceThread s = forever $ do 
            p <- waitForPresence (\p -> presenceType p == Subscribe) s
            putStrLn (show p)

messageThread :: Session -> IO ()
messageThread s = forever $ do 
            msg <- getMessage s
            putStrLn (show msg)  

main = do
        (sess', error) <- login "jabber.se" "zydeon" "olecas"
        let sess = fromJust sess'
        b <- getBuddies sess
        print bq
        
        -- create threads
        presenceThread sess
        --messageThread  sess


