{-
 - The driver file for running the tests via "cabal test". This is slightly
 - different to how we might run them from the command line, since suppressing
 - ANSI output is forced in this case (otherwise the log file is completely
 - messed up).
 -}

module Main (main) where

import Data.Monoid (mempty)
import Test.Framework

import ChessTools.Test.Suite (tests)


main :: IO ()
main = defaultMainWithOpts tests options
    where options = (mempty :: RunnerOptions) {ropt_plain_output = Just True}

