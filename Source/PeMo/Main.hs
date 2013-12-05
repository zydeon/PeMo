{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Xmpp
import Network.Socket
import Data.Text.Internal
import Data.Text
import Control.Monad
import Data.Default
import System.Log.Logger
import Data.Maybe
import IMNetwork

main = do
        (sess', error) <- login "jabber.se" "zydeon" "olecas"
        let sess = fromJust sess'
        forever $ do
            --p <- waitForPresence (\p -> presenceType p == Subscribe) sess
            --putStrLn (show p)
            msg <- getMessage sess
            putStrLn (show msg)



