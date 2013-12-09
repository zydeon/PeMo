{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Concurrent
import Control.Monad
import Types
import qualified Data.Text as T

-- TODO remove this
import Network.Xmpp hiding (Jid)

uiInit :: Chan IMEvent -> Chan UIEvent -> IO ()
uiInit cIM cUI = do

  chat    <- multiLineEditWidget
  typing  <- editWidget
  buddies <- multiLineEditWidget
  
  typing `onActivate` \this -> do
                               text <- getEditText this
                               sendOnSendEv cUI (parseJid "mozhan@jabber.se") text
                               updateText chat text
                               setEditText this ""


  fg <- newFocusGroup
  addToFocusGroup fg typing

  ui <- (bordered chat)
        <--> (plainText "Commands: EXIT= Esc   ... ")
        <--> (bordered typing)

  setBoxChildSizePolicy ui (Percentage 88)
  
  bigBox  <- (bordered ui)
          <++> (bordered buddies)
          
  setBoxChildSizePolicy bigBox (Percentage 85)

  coll <- newCollection
  _ <- addToCollection coll bigBox fg

  fg `onKeyPressed` \_ k _ ->
      case k of
        KEsc -> shutdownUi >> return True
        _ -> return False

  forkIO $ listenThread chat cIM
  runUi coll $ defaultContext { focusAttr = fgColor blue }

-- send onSend event to channel
sendOnSendEv :: Chan UIEvent -> Jid -> Text -> IO ()
sendOnSendEv c j t = writeChan c (OnSend j t)

listenThread :: Widget Edit -> Chan IMEvent -> IO ()
listenThread w ch = forever $ do
        ev <- readChan ch
        case ev of 
            (OnMessage jid text) -> schedule $ updateText w text
            _                    -> return ()


updateText :: Widget Edit -> Text -> IO ()
updateText w t = do 
                oldTxt <- getEditText w
                if (T.unpack oldTxt) == ""
                   then
                      setEditText w $ T.pack (": " ++  T.unpack t)
                   else
                      setEditText w $ T.pack $ ((T.unpack oldTxt) ++ "\n: " ++ (T.unpack t))
