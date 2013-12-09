{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Types
import UI
import IM

main = do
    uiChan <- newChan  -- UI -> IM  (UIEvents)
    imChan <- newChan  -- IM -> UI  (IMEvents)
    forkIO $ imInit imChan uiChan
    uiInit imChan uiChan