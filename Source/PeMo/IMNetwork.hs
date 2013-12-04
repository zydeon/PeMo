module IMNetwork where

import Network.Xmpp
import Network.Socket
import Data.Text.Internal
import Data.Default
--import Control.Monad
--import System.Log.Logger    


-- Login to domain. If successful returns True and
-- makes a session with server, otherwise returns False
makeSession :: HostName -> Text -> Text -> IO (Either XmppFailure Session)
makeSession domain user pass = session domain
                                 (Just (\_ -> ( [scramSha1 user Nothing pass]), Nothing))
                                 def


setState :: Bool -> Session -> IO Bool
setState b s = sendPresence (Presence Nothing Nothing Nothing Nothing state []) s
            where state | b == True = Available
                        | otherwise  = Unavailable



logout :: Session -> IO ()
logout s = do
              setState False s
              endSession s
            
