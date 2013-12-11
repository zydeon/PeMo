{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import System.Exit
import Types
import UI
import IM
import Util


main = do
     putStrLn "Welcome To PeMo Messenger!"    
     mSession <- mkSession
     case mSession of
         Nothing             -> do
                             putStrLn "Goodbye!"
                             exitWith ExitSuccess
         Just (session, jid) -> initChat session jid


initChat :: Session -> Jid -> IO ()
initChat s jid = do 
               uiChan <- newChan  -- (UIActions)
               imChan <- newChan  -- (IMActions)
               -- initiate both threads UI and IM
               forkIO $ imInit imChan uiChan s
               uiInit imChan uiChan jid

