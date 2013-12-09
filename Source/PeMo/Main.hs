{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Types
import UI
import IM
import System.Posix.Terminal 
import System.Posix.IO (stdInput)

main = do
    getCreds
    uiChan <- newChan  -- UI -> IM  (UIEvents)
    imChan <- newChan  -- IM -> UI  (IMEvents)
    forkIO $ imInit imChan uiChan
    uiInit imChan uiChan


-- Collects the user credentials from the command line.
getCreds :: IO (String, String, String)
getCreds = do
  putStrLn "Userame:"
  usr <- getLine
  putStrLn "Password:"
  tc <- getTerminalAttributes stdInput
  setTerminalAttributes stdInput (withoutMode tc EnableEcho) Immediately
  pass <- getLine
  setTerminalAttributes stdInput tc Immediately
  putStrLn "Domain:"
  domain <- getLine
  return (usr, pass, domain)
