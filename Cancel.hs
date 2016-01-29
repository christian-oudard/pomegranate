module Cancel (onCancel) where

import Control.Exception (throwTo, AsyncException(UserInterrupt))
import Control.Concurrent (ThreadId, myThreadId, threadDelay)
import System.Posix.Signals (keyboardSignal, installHandler, Handler(Catch))

-- Example:
--
-- main = do
--   onCancel $ do
--     putStrLn ""
--     putStrLn "Cancel!"
--
--   threadDelay $ 10*1000000

onCancel :: IO () -> IO Handler
onCancel action = do
  tid <- myThreadId
  installHandler keyboardSignal (handler tid) Nothing
  where
    handler tid = Catch $ do
      action
      throwTo tid UserInterrupt
