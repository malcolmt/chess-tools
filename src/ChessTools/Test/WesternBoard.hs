module ChessTools.Test.WesternBoard
where

import Data.Char (ord, chr)
import Test.QuickCheck

import ChessTools.Board.Western

pairToCoords :: Int -> Int -> [Char]
pairToCoords f r = chr (f + ord 'a') : chr (r + ord '1') : []

validAlgebraicSquaresGen :: Gen [Char]
validAlgebraicSquaresGen = do
    file <- choose (0, 7)
    rank <- choose (0, 7)
    return $ pairToCoords file rank

invalidAlgebraicSquaresGen :: Gen [Char]
invalidAlgebraicSquaresGen =
    oneof [badFile, badRank, longName]
    where badFile = do
            file <- oneof [choose (-10, -1), choose (8, 12)]
            rank <- choose (-3, 10)
            return $ pairToCoords file rank

          badRank = do
            file <- choose (-3, 10)
            rank <- oneof [choose (-10, -1), choose (8, 12)]
            return $ pairToCoords file rank

          longName = do
            x <- choose ('a', 'z')
            c <- validAlgebraicSquaresGen
            return $ c ++ [x]


prop_goodAlgebraicSquares = forAll validAlgebraicSquaresGen $ \s ->
    case algebraicToIndex s of
        Nothing -> False
        _       -> True

prop_badAlgebraicSquares = forAll invalidAlgebraicSquaresGen $ \s ->
    case algebraicToIndex s of
        Nothing -> True
        _       -> False

