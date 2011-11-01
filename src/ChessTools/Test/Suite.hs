{-
 - Grouping of tests into groups and giving them readable names.
 -
 - Runner files for these tests are in ConsoleTest.hs and CabalTest.hs.
 -}

module ChessTools.Test.Suite (
    tests
) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import ChessTools.Test.Board
import ChessTools.Test.WesternBoard


tests :: [Test]
tests = [
          testGroup "Board arrays" [
              testProperty "index to square" prop_indexToSquareInverse
            , testProperty "square to index" prop_squareToIndexInverse
            , testProperty "bad square to index" prop_errorSquareToIndex
            , testProperty "bad index to square" prop_errorIndexToSquare
            , testProperty "indices increase" prop_indexIncreasesWithSquare
            , testProperty "array size" prop_boardArraySize
            ]
        , testGroup "Lookup arrays" [
              testProperty "repIndexList represents" prop_repIndexListRepresents
            , testProperty "file lookup 1" prop_checkFileDistance1
            , testProperty "file lookup 2" prop_checkFileDistance2
            , testProperty "rank lookup 1" prop_checkRankDistance1
            , testProperty "rank lookup 2" prop_checkRankDistance2
            , testProperty "square lookup 1" prop_checkSquareDistance1
            , testProperty "square lookup 2" prop_checkSquareDistance2
            ]
        , testGroup "Western notation" [
              testProperty "good algebraic squares" prop_goodAlgebraicSquares
            , testProperty "bad algebraic squares" prop_badAlgebraicSquares
            , testProperty "index to algebraic" prop_indexToAlgebraic
            , testProperty "bad index to algebraic" prop_errorIndexToAlgebraic
            ]
        ]

