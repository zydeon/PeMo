module GUI
(
  GUI
, loop
, mkGUI
, collection
, context
, mkTypingUI
, addToGUI
 )
where


import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Data.IORef
import Control.Monad

type Text = T.Text
data GUI = GUI {collection :: IO Collection, context :: RenderContext}

mkGUI :: IO GUI
mkGUI = return GUI {  collection = newCollection
                     ,  context    = defaultContext
                     }


-- returns typing widget and associated to given focus group
mkTypingUI :: Widget FocusGroup -> (Text -> IO ()) -> IO Widget (VCentered (HCentered Edit))
mkTypingUI fg f = do
            e  <- editWidget
            e `onActivate` \this -> getEditText this >>= f
            ui <- centered e            
            addToFocusGroup fg e
            return ui

addToGUI :: GUI -> (Widget (VCentered (HCentered a)), Widget FocusGroup) -> IO GUI
addToGUI g (ui, fg) = do 
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