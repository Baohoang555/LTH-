module Utils.Concurrency (withGameLock) where

import Control.Concurrent (MVar, modifyMVar)

-- Helper để lock và modify MVar an toàn
withGameLock :: MVar a -> (a -> IO (a, b)) -> IO b
withGameLock = modifyMVar