{- | Semi-private internal implementation features used by 'ChessTools.Board'.

   Most things in here are only useful in the public module. However, in case
   of unanticipated needs, it is possible to use these directly.
 -}
module ChessTools.Board.Internal
where

import Data.Array

-- | A structure encapsulting the useful parts of a board size. This won't
-- change for a particular sort of game (such as western chess or Shogi or
-- Xianqi).
--
-- The row and column length values are obvious. The vertical buffer size is
-- used internally for creating various data structures and depends on the
-- pieces being used in the game (hence must be provided by the library user).
-- It is the maximum distance any jumping piece could move off the top or
-- bottom of the board. In western-style chess, this will be 2, since a knight
-- can jump that far from either the 1st or 8th ranks.
data BoardSize = BoardSize {
                    boardHorizSize :: Int,   -- ^ Row length.
                    boardVertSize :: Int,    -- ^ Column length.
                    boardVertBuffer :: Int   -- ^ Vertical buffer size for
                                             --   jumping pieces.
                    }
                    deriving (Show, Eq)

-- | The coordinates of a cell on the board.
--
-- 'Square' coordinates are 0-based, with the column preceeding the rank So the
-- first row of a western chess board is (0, 0) (the /a1/ square) to (0, 7),
-- with the /h8/ square having coordinates (7, 7).
--
-- Note that the order of coordinates in a `Square` is independent of any
-- traditional external representation of a game's cells. Thus, whilst Shogi
-- games are recorded with the rank before the column (for example, \"/7f/\"),
-- this module and its callers still use column-first ordering.
newtype Square = Square (Int, Int) deriving (Show, Eq)

-- There are times when it is convenient to be able to sort a list of squares,
-- so we impose a somewhat arbitrary ordering on them. This is not the
-- automatically derived version, because we want to sort by ranks ahead of
-- files.
instance Ord Square where
    compare (Square (x1, y1)) (Square (x2, y2)) =
        case y1 `compare` y2 of
            LT -> LT
            GT -> GT
            EQ -> x1 `compare` x2


-- | A representation of a board position.
newtype Board = Board (Array BIndex Int) deriving (Show, Eq)

-- | An index into a 'Board' array.
newtype BIndex = BI {fromBI :: Int} deriving (Show, Eq, Ord, Ix)

-- | Subtract two board indexes to get an offset into a lookup table.
biMinus :: BIndex -> BIndex -> LIndex
biMinus x y = LI $ fromBI x - fromBI y

-- | An index into a 'LookupTable' array.
newtype LIndex = LI {fromLI :: Int} deriving (Show, Eq, Ord, Ix)

-- | A rapid lookup (/O(1)/) data structure for computing various values based
-- on two squares on the board. These could be distances between the squares in
-- some form (file or rank separation) or whether some kind of piece can move
-- between those two squares (using 0 values for invalid moves).
data LookupTable = LookupTable (Array LIndex Int)

-- | Used to hold a representative set of squares when computing 'LookupTable'
-- results. Create one with 'repIndexList' and use it in all lookup table
-- creation functions.
--
-- For internal code using this, the main invariant to note is that the
-- 'LIndex' component is in sorted order. This is used by, for example, the
-- 'lookupBounds' function.
newtype CoveringIndexList = CL [(LIndex, (Square, Square))]

-- | Convert a 'Square' to an index into a board array. The index is the same
-- for all board arrays associated with a given 'BoardSize'.
squareToIndex :: BoardSize -> Square -> BIndex
squareToIndex s (Square (x, y))
    | x < 0 || y < 0 || x >= h || y >= v = BI 0
    | otherwise = BI $ (y + vBuf) * rowLength s + leftBuf s + x
    where BoardSize h v vBuf = s

-- | Convert a board array index to a 'Square'. This is the inverse of
-- 'squareToIndex'.
indexToSquare :: BoardSize -> BIndex -> Square
indexToSquare s (BI idx) = Square (x, y)
    where rl = rowLength s
          idx' = idx - rl * boardVertBuffer s
          (y, x') = idx' `divMod` rl
          x = x' - leftBuf s

-- | The length of a single (virtual) row in the board array. This is wider
-- than the board row length due to the buffer space at each end of the row.
rowLength :: BoardSize -> Int
rowLength s = 2 * boardHorizSize s - 1

-- | The amount of buffer space at the start of each virtual row in the board
-- array before the board data proper begins.
leftBuf :: BoardSize -> Int
leftBuf s = boardHorizSize s `div` 2

-- | Returns a pair that can be used to specify the bounds for a
-- 'Data.Array.Array'.
lookupBounds :: CoveringIndexList -> (LIndex, LIndex)
lookupBounds (CL cs) = (fst $ head cs, fst $ last cs)

-- | Utility function for computing the file, rank and square distance tables.
-- In each case, the computations are almost identical, differing only by the
-- type of calculation performed on the two squares. That calculation function
-- is passed in as the first argument to 'distanceTableWith'.
distanceTableWith :: (Square -> Square -> Int) -> CoveringIndexList ->
                      LookupTable
distanceTableWith cmp cl@(CL cs) =
    LookupTable (array (lookupBounds cl) $ map f cs)
    where f (d, (sq1, sq2)) = (d, cmp sq1 sq2)

