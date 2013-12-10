{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Concurrent
import Control.Monad
import Types
import qualified Data.Text as T


-- delete after
import Network.Xmpp (parseJid, jidToText)


type ConvWindow'  = (Box (Box (Bordered Edit) FormattedText) (Bordered Edit))
data Conversation = Conversation { widget :: Widget Edit, showC :: IO ()}
data State = State { conversations :: [(Jid, Conversation)]
                   , activeBuddy   :: Maybe Jid
                   }--, contacts :: [(Jid, PresenceType)] }

newState :: State
newState = State { conversations = [], activeBuddy = Nothing }

openConv :: Chan UIAction -> ActivateItemEvent Jid b -> IO ()
openConv ch (ActivateItemEvent _ a b) = writeChan ch (OpenConver a)

uiInit :: Chan IMAction -> Chan UIAction -> Jid -> IO ()
uiInit cIM cUI myJid = do
  conversations <- newGroup
  buddies       <- multiLineEditWidget

  chat      <- multiLineEditWidget
  typing    <- editWidget
  buddyList <- newList (fgColor white)

  ---------------------------------------------------------------
  -- Hard coded buddies on the list:
  let m = "mozhan@jabber.se" in addToList buddyList (parseJid (T.unpack m))  =<< plainText m
  let z = "zydeon" in addToList buddyList (parseJid (T.unpack z)) =<< plainText z
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  let y = "a" in addToList buddyList (parseJid (T.unpack y)) =<< plainText y
  ---------------------------------------------------------------

  onItemActivated buddyList (openConv cUI)

  fg <- newFocusGroup
  addToFocusGroup fg buddyList

  c <- (bordered chat)
                <--> (plainTextWithAttrs [(("Commands: Exit = Esc , Switch = Tab , List Navigation: Up/Down"), fgColor green)])
                <--> (bordered typing)
  setBoxChildSizePolicy c (Percentage 88)
  _ <- addToGroup conversations c



  bigBox  <-   ((plainTextWithAttrs [(("\n PeMo Messenger! "), fgColor green)]))
               <--> (bordered conversations)
               <++> ((plainTextWithAttrs [(("\n Buddies: "), fgColor green)])
               <--> (bordered buddyList))
          
  setBoxChildSizePolicy bigBox (Percentage 80)

  coll <- newCollection
  _ <- addToCollection coll bigBox fg

  fg `onKeyPressed` \_ k _ ->
      case k of
        KEsc -> shutdownUi >> return True
        _    -> return False

  forkIO $ listenThread newState conversations fg cIM cUI myJid
  runUi coll $ defaultContext { focusAttr = fgColor blue }

-- send onSend event to channel
sendOnSendEv :: Chan IMAction -> Jid -> Text -> IO ()
sendOnSendEv c j t = writeChan c (SendMsg j t)

-- create conversation. Returns action that, when evaluated, 
-- forces the corresponding conversation to appear
mkConversation :: Jid -> Widget (Group ConvWindow') -> Widget FocusGroup -> Chan IMAction -> Jid -> IO Conversation
mkConversation j convs fg cIM myJid = do
              c <- multiLineEditWidget  -- conversation widget
              t <- editWidget           -- typing widget
              t `onActivate` \this -> do 
                                    text <- getEditText this
                                    sendOnSendEv cIM j text
                                    setEditText this ""
                                    updateText c text myJid


              schedule $ void $ addToFocusGroup fg t
              conv <- (bordered c)
                      <--> (plainTextWithAttrs [(("Commands: Exit = Esc , Switch = Tab , List Navigation: Up/Down"), fgColor green)])
                      <--> (bordered t)
              setBoxChildSizePolicy conv (Percentage 88)
              show_ <- addToGroup convs conv
              return Conversation{widget = c, showC = show_}

-- receives current state, group of conversations and channel to communicate
listenThread :: State -> Widget (Group ConvWindow') -> Widget FocusGroup -> Chan IMAction -> Chan UIAction -> Jid -> IO ()
listenThread s convs fg cIM cUI myJid = do
        ev <- readChan cUI
        case ev of 
            (DisplayMsg jid text) -> do
                  mc <- lookupC (conversations s) jid
                  case mc of
                    Nothing -> do 
                        c <- mkConversation jid convs fg cIM myJid
                        schedule $ do
                            updateText (widget c) text jid
                        showC c
                        cs' <- insertC (conversations s) jid c
                        let s' = State{conversations=cs', activeBuddy=(activeBuddy s)}
                        listenThread s' convs fg cIM cUI myJid

                    Just c  -> do 
                        schedule $ updateText (widget c) text jid
                        showC c
                        listenThread s convs fg cIM cUI myJid
                      --if (activeBuddy s) /= jid
                      --then do return () -- change to update buddy icon
                      --else do return ()


            (OpenConver jid)     -> do
                  mc <- lookupC (conversations s) jid
                  case mc of
                    Nothing -> do 
                        c <- mkConversation jid convs fg cIM myJid
                        showC c
                        cs' <- insertC (conversations s) jid c
                        let s' = State{conversations=cs', activeBuddy=(Just jid)}
                        listenThread s' convs fg cIM cUI myJid

                    Just c  -> do 
                        showC c
                        listenThread s convs fg cIM cUI myJid
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


updateText :: Widget Edit -> Text -> Jid -> IO ()
updateText w t j = do 
                oldTxt' <- getEditText w
                let oldTxt = T.unpack oldTxt'
                if (oldTxt == "")
                  then
                    setEditText w $ T.pack $ oldTxt ++ (T.unpack $ jidToText j) ++ ": " ++ (T.unpack t)
                  else
                    setEditText w $ T.pack $ oldTxt ++ "\n" ++ (T.unpack $ jidToText j) ++ ": " ++ (T.unpack t)
                return ()
