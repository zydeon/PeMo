module GUISample where

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import qualified Data.Text as T
import System.Exit

makeWid :: IO ()
makeWid = do

  fg <- newFocusGroup -- Creates a fg widget, which will contain ordered sequece of widgs, between which we can cycle.
  fg `onKeyPressed` \_ key _ ->
                        if key == KASCII 'q'
                        then exitSuccess
                        else return False  

  e1 <- multiLineEditWidget
  e2 <- multiLineEditWidget
  addToFocusGroup fg e1
  addToFocusGroup fg e2


  ui <- vBox e1 e2


  c <- newCollection   -- Makes a new collection, containing widgs, which in turn are associated to their f.groups.
  _ <- addToCollection  c ui fg -- Adds ui (containing e) to the collection, assigns fg to it's f.group.


  runUi c $ defaultContext {focusAttr = fgColor yellow} -- runs the main event loop with the colleciton.
