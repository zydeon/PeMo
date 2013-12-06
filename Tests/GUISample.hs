module GUISample where

import Graphics.Vty.Widgets.All
import qualified Data.Text as T

makeWid :: IO ()
makeWid = do
  e  <- editWidget  -- Makes an editing widget
  ui <- centered e  -- Makes a new widget, containing e on the inside, centeres the widget vertically.
  e `onActivate` \this ->getEditText this >>= (error . ("you entered: " ++ ) . T.unpack)

  fg <- newFocusGroup -- Creates a fg widget, which will contain ordered sequece of widgs, between which we can cycle.
  addToFocusGroup fg e -- Adds the edit widg. to the f.group. The focus will be based on the order of adding widgs.

  c <- newCollection   -- Makes a new collection, containing widgs, which in turn are associated to their f.groups.
  _ <- addToCollection  c ui fg -- Adds ui (containing e) to the collection, assigns fg to it's f.group.
                                -- Upon receiving users' input, ui will be displayed.


  runUi c defaultContext -- runs the main event loop with the colleciton.
