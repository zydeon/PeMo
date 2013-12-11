module Util where

import System.Posix.Terminal 
import System.Posix.IO (stdInput)
import Types
import IM
import Data.Maybe

mkSession :: IO (Maybe (Session, Jid))
mkSession = do
    try <- tryLogin
    case try of 
        (Left  failure) -> loginFailed
        (Right session) -> do
                            mjid <- getJid' session
                            return $ Just (session, fromJust mjid)


loginFailed :: IO (Maybe (Session, Jid))
loginFailed = do 
            putStrLn "The login try failed!\nDo you wish to try again? (y/n)"
            reply <- getLine 
            if reply == "y" || reply == "Y"
                then mkSession
                else if reply == "n" || reply == "N"
                        then return Nothing
                        else loginFailed

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

tryLogin :: IO (Either LoginFailure Session)
tryLogin = do (usr,pass,dom) <- getCreds
              login dom (pack usr) (pack pass)
