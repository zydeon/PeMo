module Main where

import Control.Concurrent
import Types
import UI
import IM

main = do
    uiChan <- newChan  -- UIEvents
    imChan <- newChan  -- IMEvents
    forkIO $ imInit imChan uiChan
    uiInit imChan uiChan