{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Concurrent
import Control.Monad
import Types
import qualified Data.Text as T

type ConvWindow'  = (Box (Box (Bordered Edit) FormattedText) (Bordered Edit))
data Conversation = Conversation { widget :: Widget Edit, showC :: IO ()}
data State = State { conversations :: [(Jid, Conversation)]
                   , activeBuddy   :: Maybe Jid
                   }--, contacts :: [(Jid, PresenceType)] }

newState :: State
newState = State { conversations = [], activeBuddy = Nothing }

uiInit :: Chan IMAction -> Chan UIAction -> IO ()
uiInit cIM cUI = do
  conversations <- newGroup
  buddies       <- multiLineEditWidget

  chat      <- multiLineEditWidget
  typing    <- editWidget
  buddyList <- newList (fgColor blue)

  ---------------------------------------------------------------
  -- Hard coded buddies on the list:
  let m = "mozhan" in addToList buddyList m  =<< plainText m
  let z = "zydeon" in addToList buddyList z  =<< plainText z
  ---------------------------------------------------------------

  fg <- newFocusGroup
  addToFocusGroup fg buddyList

  c <- (bordered chat)
                <--> (plainText "Commands: EXIT= Esc   ... ")
                <--> (bordered typing)
  setBoxChildSizePolicy c (Percentage 88)
  _ <- addToGroup conversations c
  
  bigBox  <-   (bordered conversations)
               <++> ((plainTextWithAttrs [(("\n Buddies: "), fgColor green)])
               <--> (bordered buddyList))
          
  setBoxChildSizePolicy bigBox (Percentage 80)

  coll <- newCollection
  _ <- addToCollection coll bigBox fg

  fg `onKeyPressed` \_ k _ ->
      case k of
        KEsc -> shutdownUi >> return True
        _    -> return False

  forkIO $ listenThread newState conversations fg cIM cUI
  runUi coll $ defaultContext { focusAttr = fgColor blue }

-- send onSend event to channel
sendOnSendEv :: Chan IMAction -> Jid -> Text -> IO ()
sendOnSendEv c j t = writeChan c (SendMsg j t)

-- create conversation. Returns action that, when evaluated, 
-- forces the corresponding conversation to appear
mkConversation :: Jid -> Widget (Group ConvWindow') -> Widget FocusGroup -> Chan IMAction -> IO Conversation
mkConversation j convs fg cIM = do
              c <- multiLineEditWidget  -- conversation widget
              t <- editWidget           -- typing widget
              t `onActivate` \this -> getEditText this
                                         >>= sendOnSendEv cIM j
                                         >> setEditText this ""
              --schedule $ void $ addToFocusGroup fg t
              conv <- (bordered c)
                      <--> (plainText "Commands: EXIT= Esc   ... ")
                      <--> (bordered t)
              setBoxChildSizePolicy conv (Percentage 88)
              show_ <- addToGroup convs conv
              return Conversation{widget = c, showC = show_}

-- receives current state, group of conversations and channel to communicate
listenThread :: State -> Widget (Group ConvWindow') -> Widget FocusGroup -> Chan IMAction -> Chan UIAction -> IO ()
listenThread s convs fg cIM cUI = do
        ev <- readChan cUI
        case ev of 
            (DisplayMsg jid text) -> do
                  mc <- lookupC (conversations s) jid
                  case mc of
                    Nothing -> do 
                        c <- mkConversation jid convs fg cIM
                        schedule $ do
                            updateText (widget c) text
                        showC c
                        cs' <- insertC (conversations s) jid c
                        let s' = State{conversations=cs', activeBuddy=(activeBuddy s)}
                        listenThread s' convs fg cIM cUI

                    Just c  -> do 
                        schedule $ updateText (widget c) text
                        showC c
                        listenThread s convs fg cIM cUI
                      --if (activeBuddy s) /= jid
                      --then do return () -- change to update buddy icon
                      --else do return ()



            _                    -> return ()

-- Maps jid's to corresponding widgets. returns nothing if jid is not in the map
lookupC :: [(Jid, Conversation)] -> Jid -> IO (Maybe Conversation)
lookupC [] _                      = return Nothing
lookupC ((j, c):ss) j'| j == j'   = return $ Just c
                      | otherwise = lookupC ss j'

-- jid is to assumed not to be in the map
insertC :: [(Jid, Conversation)] -> Jid -> Conversation -> IO [(Jid, Conversation)]
insertC m j c = return ((j,c):m)


updateText :: Widget Edit -> Text -> IO ()
updateText w t = do 
                oldTxt <- getEditText w
                if (T.unpack oldTxt) == ""
                   then
                      setEditText w $ T.pack (": " ++  T.unpack t)
                   else
                      setEditText w $ T.pack $ ((T.unpack oldTxt) ++ "\n: " ++ (T.unpack t))
