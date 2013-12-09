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


selAttr = black `on` yellow

    

uiInit :: Chan IMEvent -> Chan UIEvent -> IO ()
uiInit cIM cUI = do

  chat      <- multiLineEditWidget
  typing    <- editWidget
  buddyList <- newList (fgColor blue)

  ---------------------------------------------------------------
  -- Hard coded buddies on the list:
  let m = "mozhan" in addToList buddyList m  =<< plainText m
  let z = "zydeon" in addToList buddyList z  =<< plainText z
  ---------------------------------------------------------------
  typing `onActivate` \this -> do
                               text <- getEditText this
                               sendOnSendEv cUI (parseJid "mozhan@jabber.se") text
                               updateText chat text
                               setEditText this ""


  fg <- newFocusGroup
  addToFocusGroup fg typing
  addToFocusGroup fg buddyList

  ui <- (plainTextWithAttrs [(("PeMo Messenger! "), fgColor green)])
        <--> (bordered chat)
        <--> (plainTextWithAttrs [(("Commands: Exit = Esc , Switch = Tab , List Navigation: Up/Down"), fgColor green)])
        <--> (bordered typing)

  setBoxChildSizePolicy ui (Percentage 88)
  
  bigBox  <-   (bordered ui)
               <++> ((plainTextWithAttrs [(("\n Buddies: "), fgColor green)])
               <--> (bordered buddyList))
          
  setBoxChildSizePolicy bigBox (Percentage 80)


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
