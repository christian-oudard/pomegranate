import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Random.MWC (createSystemRandom)
import System.Random.MWC.Distributions (normal)

import Cancel (onCancel)

deviationRatio = 1/8
defaultWaitMinutes = 30

main = do
  -- Determine how long to wait for.
  timerMinutes <- parseDelay
  putStrLn $ "Timer set for about " ++ show (round timerMinutes) ++ "m."

  g <- createSystemRandom
  x <- normal (timerMinutes*60) (timerMinutes*60*deviationRatio) g

  -- Wait for the specified amount of time, handling early cancellation gracefully.
  start <- getCurrentTime

  onCancel $ do
    putStrLn ""
    end <- getCurrentTime
    let diff = diffUTCTime end start
     in putStrLn $ "Cancelled after " ++ showMinSec diff ++ "."

  delaySeconds x
  end <- getCurrentTime
  let diff = diffUTCTime end start
   in putStrLn $ "Done after " ++ showMinSec diff ++ "."

parseDelay :: IO (Double)
parseDelay = do
  args <- getArgs
  if (length args == 0)
    then do
      return defaultWaitMinutes
    else do
      return $ read (args !! 0)

delaySeconds :: RealFrac a => a -> IO()
delaySeconds s = threadDelay $ round $ s*second
  where second = 1000000

showMinSec :: (RealFrac a, Show a) => a -> String
showMinSec seconds =
  let (m, s') = divMod' seconds 60
      s = round s'
   in show m ++ "m" ++ show s ++ "s"

div' :: (Real a, Integral b) => a -> a -> b
div' n d = floor ((toRational n) / (toRational d))

mod' :: (Real a) => a -> a -> a
mod' n d = n - (fromInteger f) * d where f = div' n d

divMod' :: (Real a, Integral b) => a -> a -> (b,a)
divMod' n d = (div' n d, mod' n d)
