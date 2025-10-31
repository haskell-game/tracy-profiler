{-# LANGUAGE CPP #-}               -- __LINE__ and __FILE__ macros
{-# LANGUAGE MagicHash #-}         -- "static strings"#
{-# LANGUAGE OverloadedLabels #-}  -- #fuchsia colors
{-# LANGUAGE OverloadedStrings #-} -- "yes"

module Main where

import qualified System.Tracy as Tracy
import qualified Data.Text as Text

main :: IO ()
main = Tracy.withProfiler $ do
  Tracy.waitConnected_       -- wait for the tracy-capture to connect
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
