{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import Control.Concurrent.Chan
import Control.Concurrent (forkIO)
import Control.Monad
import Types
import Network.Xmpp hiding (Jid)
import Data.Maybe (fromJust)


type ConvWindow  = (Box (Box (Bordered Edit) FormattedText) (Bordered Edit))
data Conversation = Conversation { widget :: Widget Edit, showC :: IO ()}


-- Map of current opened converstations and pointer to current chat buddy
data State = State { conversations :: [(Jid, Conversation)]
                   , activeBuddy   :: Maybe Jid
                   , convGroup     :: Widget (Group ConvWindow)
                   , focusGroup    :: Widget FocusGroup
                   , imChannel     :: Chan IMAction
                   , uiChannel     :: Chan UIAction
                   , myJid         :: Jid
                   }

-- Creates new state
newState :: [(Jid, Conversation)]
         -> Maybe Jid
         -> Widget (Group ConvWindow)
         -> Widget FocusGroup
         -> Chan IMAction
         -> Chan UIAction
         -> Jid
         -> State
newState convs mab cg fg cIM cUI j =
           State { conversations = convs
                 , activeBuddy = mab
                 , convGroup = cg
                 , focusGroup = fg
                 , imChannel = cIM
                 , uiChannel = cUI
                 , myJid = j
                 }


-- Sends action through UI channel (because it is on a different thread)
openConv :: Chan UIAction -> ActivateItemEvent Jid b -> IO ()
openConv ch (ActivateItemEvent _ a b) = writeChan ch (OpenConver a)


-- creates UI and initiates threads
uiInit :: Chan IMAction -> Chan UIAction -> Jid -> IO ()
uiInit cIM cUI myJid = do
  (cg, fg, gui) <- createUI cIM cUI
  forkIO $ listenThread $ newState [] Nothing cg fg cIM cUI myJid
  runUi gui $ defaultContext { focusAttr = fgColor blue }


-- Creates UI
createUI :: Chan IMAction
         -> Chan UIAction
         -> IO (Widget (Group ConvWindow), Widget FocusGroup, Collection)
createUI cIM cUI = do 
        -- widgets creation
        conversations <- newGroup             -- groups layers of ConvWindow's
        buddies       <- multiLineEditWidget
        chat          <- multiLineEditWidget
        typing        <- editWidget
        buddyList     <- newList (fgColor yellow)
        fg            <- newFocusGroup        -- responsible for changing input 
                                              -- between widgets

        mapM_ (\h ->
               addToList buddyList (parseJid h) =<< plainText (pack h))
              ["mozhan@jabber.se", "zydeon@jabber.se", "zydeon2@jabber.se"]
        ---------------------------------------------------------------

        onItemActivated buddyList (openConv cUI)
        addToFocusGroup fg buddyList

        defaultConv <- mkConvWindow chat typing
        _ <- addToGroup conversations defaultConv

        ui  <- ((plainTextWithAttrs [(("\n PeMo Messenger! "), fgColor green)]))
                   <--> (bordered conversations)
                   <++> ((plainTextWithAttrs [(("\n Buddies: "), fgColor green)])
                   <--> (bordered buddyList))
                
        setBoxChildSizePolicy ui (Percentage 80)

        gui <- newCollection
        _ <- addToCollection gui ui fg

        -- Event Handler
        fg `onKeyPressed` \_ k _ ->
            case k of
              KEsc -> writeChan cIM Logout >> shutdownUi >> return True
              _    -> return False

        return (conversations, fg, gui)


-- Creates conversation window in UI
mkConvWindow :: Widget Edit -> Widget Edit -> IO (Widget ConvWindow)
mkConvWindow chat typing = do 
                      window <- (bordered chat)
                             <--> (plainTextWithAttrs [
                                  (pack ("Commands: Exit = Esc , Switch = Tab"
                                      ++ " , List Navigation: Up/Down")
                                  , fgColor green)
                                  ])
                             <--> (bordered typing)
                      setBoxChildSizePolicy window (Percentage 88)
                      return window



-- Creates conversation. Returns action that, when evaluated, 
-- forces the corresponding conversation to appear and be focused
mkConversation :: Jid
               -> Widget (Group ConvWindow)
               -> Widget FocusGroup
               -> Chan IMAction
               -> Jid
               -> IO Conversation
mkConversation j convs fg cIM myJid = do
              c <- multiLineEditWidget  -- conversation widget
              t <- editWidget           -- typing widget

              -- when pressing Enter
              t `onActivate` \this -> do 
                                    text <- getEditText this
                                    writeChan cIM (SendMsg j text)
                                    setEditText this ""
                                    updateText c text myJid

              schedule $ void $ addToFocusGroup fg t
              conv <- mkConvWindow c t
              show_ <- addToGroup convs conv
              return Conversation{ widget = c,
                                   showC = do show_
                                              schedule $ focus t
                                 }


-- Waits on UI channel for an action executes it
-- Maintains the state of opened conversations
listenThread :: State -> IO ()
listenThread st = do
        ev <- readChan (uiChannel st)
        case ev of 
            (DisplayMsg jid text) -> displayConv st jid $ Just text
            (OpenConver jid)      -> displayConv st jid   Nothing
            _                     -> return ()


-- displays a conversation
displayConv :: State        
            -> Jid
            -> Maybe Text
            -> IO ()
displayConv st jid mtext = do 
        mc <- lookupC (conversations st) jid  
        -- check if conversation is already opened
        case mc of
          Nothing -> do             -- not open
              c <- mkConversation jid
                                  (convGroup st)
                                  (focusGroup st)
                                  (imChannel st)
                                  (myJid st)
              showC c
              if mtext /= Nothing
                then schedule $ updateText (widget c) (fromJust mtext) jid
                else return ()
              cs' <- insertC (conversations st) jid c

              let st' = newState cs'
                                 (Just jid) 
                                 (convGroup st)
                                 (focusGroup st)
                                 (imChannel st)
                                 (uiChannel st)
                                 (myJid st)                                 
              listenThread st'

          Just c  -> do            -- already open
              if mtext /= Nothing
                then schedule $ updateText (widget c) (fromJust mtext) jid
                else return ()          
              showC c
              listenThread st
            -- TODO this
            --if (activeBuddy st) /= jid
            --then do return () -- change to update buddy icon
            --else do return ()


-- Maps jid's to corresponding conversations
-- Returns nothing if jid is not in the map
lookupC :: [(Jid, Conversation)] -> Jid -> IO (Maybe Conversation)
lookupC [] _                      = return Nothing
lookupC ((j, c):ss) j'| j == j'   = return $ Just c
                      | otherwise = lookupC ss j'


-- Inserts new entry to map (jid->conversation)
-- Jid is to assumed not to be in the map
insertC :: [(Jid, Conversation)] -> Jid -> Conversation -> IO [(Jid, Conversation)]
insertC m j c = return ((j,c):m)


-- Updates text of given chat widget
updateText :: Widget Edit -> Text -> Jid -> IO ()
updateText w t j = do 
                oldTxt' <- getEditText w
                let oldTxt = unpack oldTxt'
                if (oldTxt == "")
                  then
                    setEditText w
                                $ pack
                                $ oldTxt ++ (unpack $ jidToText j)
                                  ++ ": " ++ (unpack t)
                  else
                    setEditText w
                                $ pack
                                $ oldTxt ++ "\n"
                                  ++ (unpack $ jidToText j) ++ ": " ++ (unpack t)
                return ()
