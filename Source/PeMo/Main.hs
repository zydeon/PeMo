{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Types
import UI
import IM
import System.Posix.Terminal 
import System.Posix.IO (stdInput)
import System.Exit
import Data.Maybe

import Network.Xmpp
import Data.Text


main = do
    putStrLn "Welcome To PeMo Messenger!"    
    initLoop

initLoop :: IO ()
initLoop = do
    (bool,x) <- tryInit
    if bool
        then start x
        else loginLoop


loginLoop :: IO ()
loginLoop = do
           putStrLn "The login try failed!\nDo you wish to try again? (y/n)"
           reply <- getLine 
           if reply == "y"
            then initLoop
            else if reply == "n"
                  then do  putStrLn "Goodbye!"
                           exitWith ExitSuccess
                  else   loginLoop
                 
start :: Either LoginFailure Session -> IO ()
start x = do
    uiChan <- newChan  -- (UIActions)
    imChan <- newChan  -- (IMActions)
    s <- right x
    forkIO $ imInit imChan uiChan s
    jid <- formatJid . fromJust =<< getJid' s

    uiInit imChan uiChan jid


tryInit :: IO (Bool,(Either LoginFailure Session))
tryInit = do
     (usr,pass,dom) <- getCreds
     (bool,x) <- tryLogin usr pass dom
     --(bool,x) <- tryLogin "zydeon" "olecas" "jabber.se"
     return (bool,x)
     
-- Collects the user credentials from the command line.
getCreds :: IO (String, String, String)
getCreds = do
  putStrLn "Username:"
  usr <- getLine
  putStrLn "Password:"
  tc <- getTerminalAttributes stdInput
  setTerminalAttributes stdInput (withoutMode tc EnableEcho) Immediately
  pass <- getLine
  setTerminalAttributes stdInput tc Immediately
  putStrLn "Domain:"
  domain <- getLine
  return (usr, pass, domain)




tryLogin :: String -> String -> String -> IO (Bool,(Either LoginFailure Session))
tryLogin usr pass dom = do 
            conn <- (login dom (pack usr) (pack pass))
            case conn of
               Left e  ->  return (False,conn)     -- login failed
               Right s ->  return (True,conn)
