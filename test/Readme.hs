{-# LANGUAGE MagicHash #-}         -- "static strings"#
{-# LANGUAGE OverloadedLabels #-}  -- #fuchsia colors
{-# LANGUAGE OverloadedStrings #-} -- "yes"

module Main where

import Control.Monad (unless, when)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import qualified System.Tracy as Tracy
import qualified Data.Text as Text

main :: IO ()
main = Tracy.withProfiler $ do
  when Tracy.enabled do
    hPutStrLn stderr "Waiting for tracy-capture or tracy-profiler to connect on port 8086"
    isConnected <- Tracy.waitConnected 100000 (Just 30_000_000)
    unless isConnected $ die "Nothing connected within 30 seconds"
  Tracy.withSrcLoc "main.nowhereInParticular" #f0fa do
    Tracy.messageL "hi there"# -- static strings require no copying to be logged
    mapM_ runFrame [0..600]

runFrame :: Int -> IO ()
runFrame ix = Tracy.withSrcLoc "runFrame" #fcc do
  Tracy.frameMark_
  let factorial = product [1 .. toInteger ix]
  let !digits = length (show factorial)
  Tracy.message . Text.pack $
    -- runtime strings will require some memcpy'ng around, use sparingly on hot paths
    "!" <> show ix <> " has " <> show digits <> " digits"
  Tracy.plotInt "digits"# digits
