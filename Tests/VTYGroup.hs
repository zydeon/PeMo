module VTYGroup where

import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All

main = do
  --withoutGroup
  withGroup

withoutGroup :: IO ()
withoutGroup = do
      typing <- editWidget
      c <- (bordered typing)
      coll <- newCollection
      fg <- newFocusGroup
      addToFocusGroup fg typing
      _ <- addToCollection coll c fg

      fg `onKeyPressed` \_ k _ ->
          case k of
            KEsc -> shutdownUi >> return True
            _    -> return False

      runUi coll $ defaultContext { focusAttr = fgColor blue }

withGroup :: IO ()
withGroup = do
      group <- newGroup
      typing <- editWidget
      c <- (bordered typing)
      _ <- addToGroup group c
      coll <- newCollection
      fg <- newFocusGroup
      addToFocusGroup fg typing

      _ <- addToCollection coll group fg

      fg `onKeyPressed` \_ k _ ->
          case k of
            KEsc -> shutdownUi >> return True
            _    -> return False

      resetFocusGroup fg
      runUi coll $ defaultContext { focusAttr = fgColor blue }