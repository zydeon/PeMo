{-# LANGUAGE OverloadedStrings #-}

module UI where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

uiInit ch = do

  e1 <- multiLineEditWidget
  e2 <- editWidget
  e3 <- multiLineEditWidget
  
  fg <- newFocusGroup
 -- _  <- addToFocusGroup fg e1
  _  <- addToFocusGroup fg e2

  ui <- (bordered e1)
        <--> (plainText "Commands: EXIT= Esc   ... ")
        <--> (bordered e2)

  setBoxChildSizePolicy ui (Percentage 88)
  
  bigBox  <- (bordered ui)
          <++> (bordered e3)
          
  setBoxChildSizePolicy bigBox (Percentage 85)

  c2 <- hCentered   =<< (
                        return bigBox
                        )

       

  coll <- newCollection
 -- _ <- addToCollection coll c fg
  _ <- addToCollection coll c2 fg

  fg `onKeyPressed` \_ k _ ->
      case k of
        KEsc -> shutdownUi >> return True
        _ -> return False

  runUi coll $ defaultContext { focusAttr = fgColor blue }


