import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Random.MWC (createSystemRandom)
import System.Random.MWC.Distributions (normal)

import Cancel (onCancel)

main = do
  g <- createSystemRandom
  x <- normal (35*60) (4*60) g

  start <- getCurrentTime

  onCancel $ do
    putStrLn ""
    end <- getCurrentTime
    let diff = diffUTCTime end start
    putStrLn $ "Cancelled after " ++ showMinSec diff ++ "."

  delaySeconds x

  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ "Done after " ++ showMinSec diff ++ "."

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

