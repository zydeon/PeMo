{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Types
import UI
import IM
import System.Exit
import Data.Maybe
import Util

main = do
    putStrLn "Welcome To PeMo Messenger!"    
    mSession <- mkSession
    case mSession of
        Nothing      -> do putStrLn "Goodbye!"
                           exitWith ExitSuccess
        Just session -> initChat session


initChat :: Session -> IO ()
initChat s = do 
    uiChan <- newChan  -- (UIActions)
    imChan <- newChan  -- (IMActions)
    forkIO $ imInit imChan uiChan s
    jid <- formatJid . fromJust =<< getJid' s
    uiInit imChan uiChan jid

