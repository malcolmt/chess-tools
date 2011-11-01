{-
 - Driver file for running tests at a command line prompt. This is the standard
 - pass-through to the test-framework package.
 -
 - Refer to CabalTest.hs for the setup used when running "cabal test".
 -}

module Main (main) where

import Test.Framework (defaultMain)
import ChessTools.Test.Suite (tests)

main :: IO ()
main = defaultMain tests

