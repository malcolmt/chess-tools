module ChessTools.Test.WesternBoard
where

import Data.Char (ord, chr)
import Data.Maybe (fromJust)
import Test.QuickCheck

import ChessTools.Board.Western
import ChessTools.Test.Utils

pairToCoords :: Int -> Int -> String
pairToCoords f r = chr (f + ord 'a') : [chr (r + ord '1')]

algebraicSquaresGen :: Gen String
algebraicSquaresGen = do
    file <- choose (0, 7)
    rank <- choose (0, 7)
    return $ pairToCoords file rank

badAlgebraicSquaresGen :: Gen String
badAlgebraicSquaresGen = oneof [badFile, badRank, nameTooLong]
    where badFile = do
            file <- oneof [choose (-10, -1), choose (8, 12)]
            rank <- choose (-3, 10)
            return $ pairToCoords file rank

          badRank = do
            file <- choose (-3, 10)
            rank <- oneof [choose (-10, -1), choose (8, 12)]
            return $ pairToCoords file rank

          nameTooLong = do
            c <- algebraicSquaresGen
            return $ c ++ "X"


prop_goodAlgebraicSquares :: Property
prop_goodAlgebraicSquares = forAll algebraicSquaresGen $ \s ->
    algebraicToIndex s /= Nothing

prop_badAlgebraicSquares :: Property
prop_badAlgebraicSquares = forAll badAlgebraicSquaresGen $ \s ->
    algebraicToIndex s == Nothing

prop_indexToAlgebraic :: Property
prop_indexToAlgebraic = forAll (genIndex westernBoardSize) $ \idx ->
    let s = indexToAlgebraic idx
        (f:r:[]) = fromJust s
    in s /= Nothing &&
       length (fromJust s) == 2 &&
       'a' <= f && f <= 'h' &&
       '1' <= r && r <= '8'

prop_errorIndexToAlgebraic :: Property
prop_errorIndexToAlgebraic = forAll (genBadIndex westernBoardSize) $ \idx ->
    indexToAlgebraic idx == Nothing

