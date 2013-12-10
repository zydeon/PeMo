{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Types
import UI
import IM

main = do
    uiChan <- newChan  -- (UIActions)
    imChan <- newChan  -- (IMActions)
    forkIO $ imInit imChan uiChan
    uiInit imChan uiChan
