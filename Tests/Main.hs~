{-# LANGUAGE OverloadedStrings #-}

module Main where

import Echo
import Network.Xmpp
import Network.Socket
import Data.Text.Internal
import Data.Text
import Control.Monad
import Data.Default
import System.Log.Logger
import Data.Maybe

main = do
        sess <- makeSession "yabasta.com" "zydeon" "e22a8e90"
        sendPresence def sess
        forever $ do
            msg <- getMessage sess
            putStrLn (show msg)
            case answerMessage msg (messagePayload msg) of
                Just answer -> sendMessage answer sess
                Nothing     -> do   putStrLn "Received message with no sender."
                                    return True



