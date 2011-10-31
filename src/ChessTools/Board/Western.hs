{- | Routines for examining moves and board positions in western-style chess.
 -}

module ChessTools.Board.Western (
      algebraicToIndex
    , indexToAlgebraic
) where


import Data.Char (ord, chr)

import ChessTools.Board


westernBoardSize :: BoardSize
westernBoardSize = BoardSize 8 8 2

coveringIndices :: CoveringIndexList
coveringIndices = repIndexList westernBoardSize

-- | Converts a square name, such as /"e5"/ to an index into a board array.
-- Returns 'Nothing' if the provided string is invalid (too long or not a
-- reference to a legal square).
algebraicToIndex :: [Char] -> Maybe Int
algebraicToIndex cs
    | length cs /= 2 = Nothing
    | file < 0 || file > 7 = Nothing
    | rank < 0 || rank > 7 = Nothing
    | otherwise = Just . squareToIndex westernBoardSize $ Square (file, rank)
    where f:r:[] = cs
          file = ord f - ord 'a'
          rank = ord r - ord '1'

-- | Converts an index into a board array back into an algebraic notation
-- square designation, such as "/e5/".

-- FIXME: How to handle errors? How to even detect errors? (I don't want
-- indexToSquare having to go through Maybe all the time, since it will be
-- called all over the place.)
indexToAlgebraic :: Int -> Maybe [Char]
indexToAlgebraic x = Just $ chr (f + ord 'a') : chr (r + ord '1') : []
    where Square (f, r) = indexToSquare westernBoardSize x

-- kingMoves
-- queenMoves
-- rookMoves
-- bishopMoves
-- knightMoves
-- pawnMoves

