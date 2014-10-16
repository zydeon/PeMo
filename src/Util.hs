module Util where

import System.Posix.Terminal 
import System.Posix.IO (stdInput)
import Data.Maybe
import Types
import IM


-- Attempts to make a session from the collected user credentials
mkSession :: IO (Maybe (Session, Jid))
mkSession = do
          try <- tryLogin
          case try of 
              (Left  failure) -> loginFailed failure
              (Right session) -> do
                              mjid <- getJid' session
                              return $ Just (session, formatJid $ fromJust mjid)


-- In case login failed asks user if he wants to try again
loginFailed :: LoginFailure -> IO (Maybe (Session, Jid))
loginFailed err = do 
            putStrLn $ "The login failed!\n"++ err ++"\nDo you wish to try again? (y/n)"
            reply <- getLine 
            if reply == "y" || reply == "Y"
                then mkSession
                else if reply == "n" || reply == "N"
                         then return Nothing
                         else loginFailed err


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


-- Tries login with user credentials
tryLogin :: IO (Either LoginFailure Session)
tryLogin = do
         (usr,pass,dom) <- getCreds
         login dom (pack usr) (pack pass)

