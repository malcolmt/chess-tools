{-
 - Runs all the tests and provides a nice summary output, using the
 - test-framework library.
 -
 - Useful during development. During deployment situations, "cabal test" is
 - going to be more appropriate.
 -}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import ChessTools.Test.Board
import ChessTools.Test.WesternBoard


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
          testGroup "Board arrays" [
              testProperty "index to square" prop_indexToSquareInverse
            , testProperty "square to index" prop_squareToIndexInverse
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
            ]
        ]

