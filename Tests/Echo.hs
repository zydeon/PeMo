module Echo where

{-# LANGUAGE OverloadedStrings #-}

import Network.Xmpp
import Control.Monad
import Data.Default
import System.Log.Logger

makeSession :: 
result <- session
             "example.com"
              (Just (\_ -> ( [scramSha1 "username" Nothing "password"])
                           , Nothing))
              def
