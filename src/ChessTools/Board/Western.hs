{- | Routines for examining moves and board positions in western-style chess.
 -}

module ChessTools.Board.Western (
      algebraicToIndex
    , indexToAlgebraic
    , westernBoardSize
) where


import Data.Char (ord, chr)

import ChessTools.Board


westernBoardSize :: BoardSize
westernBoardSize = BoardSize 8 8 2

-- XXX: Not yet used; commented out to maintain warning-free build.
-- coveringIndices :: CoveringIndexList
-- coveringIndices = repIndexList westernBoardSize

-- | Converts a square name, such as /"e5"/ to an index into a board array.
-- Returns 'Nothing' if the provided string is invalid (too long or not a
-- reference to a legal square).
algebraicToIndex :: String -> Maybe BIndex
algebraicToIndex (f:r:[]) = squareToIndex westernBoardSize $ Square (file, rank)
    where file = ord f - ord 'a'
          rank = ord r - ord '1'

algebraicToIndex _ = Nothing


-- | Converts an index into a board array back into an algebraic notation
-- square designation, such as "/e5/".
indexToAlgebraic :: BIndex -> Maybe String
indexToAlgebraic x = case sq of
    Just (Square (f, r)) -> Just $ chr (f + ord 'a') : [chr (r + ord '1')]
    _                    -> Nothing
    where sq = indexToSquare westernBoardSize x

-- | Determine if a move /from/ an index /to/ another index is legal on the
-- given 'Board'.
-- legalMove :: Board -> BIndex -> BIndex -> Bool

-- kingMoves
-- queenMoves
-- rookMoves
-- bishopMoves
-- knightMoves
-- pawnMoves

