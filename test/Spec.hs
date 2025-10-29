{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module Main where

import System.Tracy qualified as Tracy
import System.Tracy.Zone qualified as Zone
import qualified System.Tracy.FFI as FFI

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef
import System.Random (randomIO)
import GHC.Exts (Ptr(..))
import Foreign.C.ConstPtr (ConstPtr(..))
import Foreign.C.String (withCStringLen)
import Data.Foldable (for_)

main :: IO ()
main = Tracy.withProfiler do
  Tracy.setThreadName "Haskell main"#
  -- TODO: connected <- Tracy.waitConnected 200000 (Just 5000000)
  Tracy.connected >>= print
  piState <- newIORef (0, 0)
  Tracy.withSrcLoc_ __LINE__ __FILE__ "main" #white do
    for_ [0 :: Int .. 255] \ix -> do
      runFrame ix piState
  Tracy.messageL "Simulation finished"#

factorial :: Integer -> Integer
factorial n = product [1..n]

runFrame :: Int -> IORef (Int, Int) -> IO ()
runFrame frameNumber piState = Tracy.withSrcLoc_ __LINE__ __FILE__ "runFrame" #red do
  Zone.color (fromIntegral frameNumber + 0x7F00) -- dynamic zone color
  update frameNumber piState
  rendering
  threadDelay 4000
  Tracy.frameMark_

update :: Int -> IORef (Int, Int) -> IO ()
update frameNumber piState = Tracy.withSrcLoc_ __LINE__ __FILE__ "update" #green do
  when (frameNumber `mod` 100 == 0) $
    putStrLn $ "Frame " <> show frameNumber
  when (frameNumber `mod` 15 == 0) $
    Tracy.withSrcLoc_ __LINE__ __FILE__ "factorial" #fuchsia do
      let n = toInteger frameNumber
      let !f = show $ factorial (10 * n)
      withCStringLen (take 100 $ "factorial of " <> show n <> " is " <> show f <> "...") \(txtPtr, txtSz) ->
        FFI.emitMessage (ConstPtr txtPtr) (fromIntegral txtSz) 0
  estimatePi piState
  physics
  threadDelay 2000

estimatePi :: IORef (Int, Int) -> IO ()
estimatePi piState = Tracy.withSrcLoc_ __LINE__ __FILE__ "estimatePi" #teal do
  replicateM_ 1000 $ do
    x <- randomIO :: IO Double
    y <- randomIO :: IO Double
    atomicModifyIORef' piState (\(inside, total) ->
      let newTotal = total + 1
          newInside = if x*x + y*y <= 1 then inside + 1 else inside
      in ((newInside, newTotal), ()))
  (inside, total) <- readIORef piState
  let !pi' = 4 * fromIntegral inside / fromIntegral total
  FFI.emitPlot (ConstPtr $ Ptr "pi\0"#) pi'

physics :: IO ()
physics = Tracy.withSrcLoc_ __LINE__ __FILE__ "physics" #blue do
  threadDelay 5000

rendering :: IO ()
rendering = Tracy.withSrcLoc_ __LINE__ __FILE__ "rendering" #yellow do
  threadDelay 5000
