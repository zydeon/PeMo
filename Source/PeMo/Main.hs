module Main where

import Control.Concurrent
import Types
import UI
import IM

main = do
    uiChan <- newChan
    imChan <- newChan
    forkIO $ imInit uiChan
    uiInit imChan