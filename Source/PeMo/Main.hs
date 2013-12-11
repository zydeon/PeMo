{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Types
import UI
import IM
import System.Exit
import Util

main = do
    putStrLn "Welcome To PeMo Messenger!"    
    mSession <- mkSession
    case mSession of
        Nothing             -> do putStrLn "Goodbye!"
                                  exitWith ExitSuccess
        Just (session, jid) -> initChat session jid


initChat :: Session -> Jid -> IO ()
initChat s jid = do 
    uiChan <- newChan  -- (UIActions)
    imChan <- newChan  -- (IMActions)
    forkIO $ imInit imChan uiChan s
    uiInit imChan uiChan jid

