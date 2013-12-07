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

import Graphics.Vty


type Interface a = (Widget a, Widget FocusGroup)
data UI = UI {
                onMessage :: Jid -> Text -> IO (),
                onPresence :: Jid -> Text -> IO ()
             }
data IM = IM { onSend :: Jid -> Text -> IO () }

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
                            "body" -> putStr $ T.unpack $ getIMText msg el
                            _      -> putStr ""


-- action to elements of type body
getIMText :: Message -> Element -> Text
getIMText m e = T.pack $ (T.unpack username) ++ ": " ++ (T.unpack body)
                where
                    username = case (msgFrom m) of
                        Nothing   -> "<>"
                        Just user -> user
                    body = case (getIMBody m) of
                        Nothing   -> "Main: not valid IM data"    
                        Just text -> text

onEnter :: Session -> IO Jid -> Text -> IO ()
onEnter s mjid t = do
                jid <- mjid
                void $ sendIM s jid t -- TODO sendIM output handling

main = do
        --uiChan <- newChan
        --imChan <- newChan
        --forkIO $ imInit uiChan
        --uiInit imChan


        (sess', error) <- login "jabber.se" "zydeon" "olecas"
        let sess = fromJust sess'
        --b <- getBuddies sess
        --print b

        -- current buddy (avoid redefining event handler for typing interface)
        buddyRef <- newIORef (parseJid "")
        writeIORef buddyRef (parseJid "mozhan@jabber.se")

        ---- create widgets
        typingW <- mkTypingW (onEnter sess (readIORef buddyRef))
        chatW   <- mkChatW

        -- create GUI
        gui <- mkGUI chatW typingW

        -- create threads
        forkIO $ messageThread sess


        loop gui

