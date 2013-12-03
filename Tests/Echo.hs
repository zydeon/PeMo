module Echo where

{-# LANGUAGE OverloadedStrings #-}

import Network.Xmpp
import Network.Socket
import Data.Text.Internal
import Control.Monad
import Data.Default
import System.Log.Logger

makeSession :: HostName -> Text -> Text -> IO Session --IO (Either XmppFailure Session)
makeSession d u p = do
                     result <- session d
                                   (Just (\_ -> ( [scramSha1 u Nothing p]), Nothing))
                                   def
                     case result of 
                        Right s -> return s
                        Left e -> error $ "XmppFailure: " ++ (show e)



