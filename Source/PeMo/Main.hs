{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp
import Network.Socket
import Data.Text.Internal
import qualified Data.Text as T
import Control.Monad
import Data.Default
import System.Log.Logger
import Data.Maybe
import IMNetwork
import Control.Concurrent (forkIO)
import Data.XML.Types 
import Graphics.Vty.Widgets.All
import GUI
import Data.IORef


presenceThread :: Session -> IO ()
presenceThread s = forever $ do 
            p <- waitForPresence (\p -> presenceType p == Subscribe) s
            putStrLn (show p)

messageThread :: Session -> IO ()
messageThread s = forever $ do 
            msg <- getMessage s
            let elements = getMsgElements msg
            sequence_ $ map (handleElem msg) elements

    where handleElem msg el = case (elementType el) of
                            "body" -> bodyAction msg el
                            _      -> putStr ""


-- action to elements of type body___ TODO: output text with encoding
bodyAction :: Message -> Element -> IO ()
bodyAction m e = putStr $ (T.unpack username) ++ ": " ++ (T.unpack body)
                where
                    username = case (msgFrom m) of
                        Nothing   -> "<>"
                        Just user -> user
                    body = case (getIMBody m) of
                        Nothing   -> "Main: not valid IM data"    
                        Just text -> text

inputThread :: Session -> IO ()
inputThread s = forever $ do
            text <- getLine
            -- TODO sendIM output handling
            void $ sendIM s (parseJid "mozhan@jabber.se") (T.pack text)

onEnter :: Session -> IO Jid -> Text -> IO ()
onEnter s mjid t = do
                jid <- mjid
                void $ sendIM s jid t -- TODO sendIM output handling

main = do
        (sess', error) <- login "jabber.se" "zydeon" "olecas"
        let sess = fromJust sess'
        --b <- getBuddies sess
        --print b

        -- current buddy (avoid redefining event handler for typing interface)
        buddyRef <- newIORef (parseJid "")
        writeIORef buddyRef (parseJid "mozhan@jabber.se")

        -- create interfaces
        typingUI <- mkTypingUI (onEnter sess (readIORef buddyRef))


        -- create GUI
        gui <- mkGUI
        gui <- addToGUI gui typingUI


        -- create threads
        forkIO $ messageThread sess

        loop gui
