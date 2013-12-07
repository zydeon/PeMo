{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

uiInit c = do

  e2 <- multiLineEditWidget
  e3 <- editWidget

  fg <- newFocusGroup
 -- _  <- addToFocusGroup fg e2
  _  <- addToFocusGroup fg e3

  ui <- (bordered e2) <--> (bordered e3)
  setBoxChildSizePolicy ui (Percentage 90)

  c <- centered   =<< (
                         (return ui)
                         <--> (plainText "Commands: EXIT= ^E  ... ")
                     --  <--> (plainText "- Esc to quit\n\n- TAB to switch editors") >>= withBoxSpacing 1

                      )

  coll <- newCollection
  _ <- addToCollection coll c fg

  fg `onKeyPressed` \_ k _ ->
      case k of
        KEsc -> shutdownUi >> return True
        _ -> return False

  runUi coll $ defaultContext { focusAttr = fgColor yellow }


