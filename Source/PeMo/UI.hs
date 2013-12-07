{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Concurrent
import Control.Monad
import Types
import qualified Data.Text as T

uiInit :: Chan IMEvent -> Chan UIEvent -> IO ()
uiInit cIM cUI = do

  chat    <- multiLineEditWidget
  typing  <- editWidget
  buddies <- multiLineEditWidget
  
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


onMessage :: Widget Edit -> Text -> IO ()
onMessage w t = setEditText w t

listenThread :: Widget Edit -> Chan IMEvent -> IO ()
listenThread w ch = forever $ do
        ev <- readChan ch
        case ev of 
            (OnMessage jid text) -> schedule $ do
                                        setEditText w text
            _                    -> return ()