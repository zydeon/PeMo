module GUISample where

import Graphics.Vty.Widgets.All
import Graphics.Vty.LLInput
import qualified Data.Text as T
import System.Exit

makeWid :: IO ()
makeWid = do
<<<<<<< HEAD
------  e  <- editWidget  -- Makes an editing widget
------  ui <- centered e  -- Makes a new widget, containing e on the inside, centeres the widget vertically.
=======
  e  <- editWidget  -- Makes an editing widget
  ui <- centered e  -- Makes a new widget, containing e on the inside, centeres the widget vertically.
  e `onActivate` \this ->getEditText this >>= (error . ("you entered: " ++ ) . T.unpack)
>>>>>>> 5c0fe1668dce6b18adb0ae7ec38f5111c7a22509

  fg <- newFocusGroup -- Creates a fg widget, which will contain ordered sequece of widgs, between which we can cycle.
------  addToFocusGroup fg e -- Adds the edit widg. to the f.group. The focus will be based on the order of adding widgs.

  c <- newCollection   -- Makes a new collection, containing widgs, which in turn are associated to their f.groups.
------  _ <- addToCollection  c ui fg -- Adds ui (containing e) to the collection, assigns fg to it's f.group.
                                -- Upon receiving users' input, ui will be displayed.

<<<<<<< HEAD
  e1 <- editWidget
  e2 <- editWidget

  interface <- vBox e1 e2
  _ <- addToCollection c interface fg
  
----- tbl `onActivate` \this ->getEditText this >>= (error . ("you entered: " ++ ) . T.unpack)
=======
>>>>>>> 5c0fe1668dce6b18adb0ae7ec38f5111c7a22509

  runUi c defaultContext -- runs the main event loop with the colleciton.
