{-# LANGUAGE OverloadedStrings #-}

module GUI
(
  GUI
, loop
, mkGUI
, collection
, context
, mkTypingW
, mkChatW
 )
where


import Graphics.Vty
import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Data.IORef
import Control.Monad
import System.Exit
import Graphics.Vty.LLInput

type Text = T.Text

type Interface a = (Widget a, Widget FocusGroup)
data GUI = GUI {
                collection :: IO Collection,
                context :: RenderContext
                }

--mkGUI :: IO GUI
--mkGUI = return GUI {  collection   = newCollection
--                        ,  context = defaultContext { focusAttr = fgColor yellow }
--                       }


-- returns typing widget and associated to given focus group
mkTypingW :: (Text -> IO ()) -> IO (Widget Edit)
mkTypingW f = do
            e  <- editWidget
            e `onActivate` \this -> getEditText this >>= f
                                    >> setEditText this ""
            --setEditLineLimit e (Just 1)
            return e

mkChatW :: IO (Widget Edit)
mkChatW = multiLineEditWidget


--mkChatW  :: (Text -> IO ()) -> IO (Widget FormattedText)
--mkChatW f = plainText ">" 

mkGUI :: Show a => Show b =>
        Widget a -> Widget b -> IO GUI
mkGUI chat typing = do
            fg <- newFocusGroup
            addToFocusGroup fg typing

            ui <- (bordered chat) <--> (bordered typing)
            fg `onKeyPressed` \_ key _ ->
                  case key of
                    KEsc -> shutdownUi >> return True
                    _ -> return False

            setBoxChildSizePolicy ui (Percentage 90)


            c <- newCollection
            _ <- addToCollection c ui fg

            return GUI {  collection = return c
                        , context    = defaultContext { focusAttr = fgColor yellow }
                       }            


addToGUI :: Show a => GUI -> Interface a -> IO GUI
addToGUI g (ui,fg) = do 
                      c <- collection g
                      _ <- addToCollection c ui fg
                      updateGUIColl g c

-- returns new GUI with new collection
updateGUIColl :: GUI -> Collection -> IO GUI
updateGUIColl g c = return GUI { collection = return c
                               , context    = (context g)
                               }

loop :: GUI -> IO ()
loop g = do
          c <- collection g
          runUi c (context g)
