{- | General routines and data structures to support generating moves in a
   chess-like game.

   Unless you are implementing support for a new style of game, you probably
   won't use this module directly. More specific and useful functions are in
   modules such as 'ChessTools.Board.Western' (for western chess, for example).
 -}

module ChessTools.Board (
    -- * Board representations
    -- $board_arrays
      BoardSize(..)
    , boardArraySize
    , Square(..)
    , squareToIndex
    , indexToSquare

    -- * Creating lookup tables
    -- $lookup_creation
    , LookupTable
    , CoveringIndexList(..)
    , repIndexList

    -- * Existing lookup creation functions
    -- $tables
    , fileTable
    , rankTable
    , squareTable

    -- * Using lookup tables
    , fetch

) where


import Data.Array ((!))
import Data.List (groupBy, sort)

import ChessTools.Board.Internal

-- $board_arrays
-- There are two types of array-like structures in this module. The
-- 'BoardArray' is a representation of a specific position in a single game. It
-- represents the state of play at a particular moment. Any references to
-- /"board"/ in this documentatin is talking about board arrays.
--
-- The other type of useful data structure are lookup arrays. They are designed
-- to assist rapid computation of various quantities concerning the
-- relationship between two squares. Functions for working with lookup arrays
-- are documented further down.

-- | Computes the size of the index array for a board of the given dimensions.
boardArraySize :: BoardSize -> Int
boardArraySize s@(BoardSize _ v vBuf) =  rowLength s * (v + 2 * vBuf)

-- $lookup_creation
-- Creating lookup tables for the various piece types in a game is a once-off
-- effort that is done before any play starts. The typical process involves
-- creating a valid 'BoardSize' and 'CoveringIndexList' and repeatedly using
-- those to create the necessary 'LookupTable' items.
--
-- For example, in a program that needs to create moves for a western chess
-- game, the setup process would look like
--
-- @
-- boardSize = BoardSize 8 8 2
-- coveringList = repIndexList boardSize
--
-- slidingTable = makeSlidingTable boardSize coveringList
--      where makeSlidingTable bs cl = ...
-- knightTable = makeKnightTable boardSize coveringList
--      where makeKnightTable bs cl = ...
-- @
--
-- This slightly verbose creation process is necessary only because creating
-- the 'CoveringIndexList' each time is time consuming. Since it remains
-- constant for a given 'BoardSize' and the board size doesn't change for a
-- particular sort of game, they can be computed once and reused.

-- | Return the 'LookupTable' value for a pair of indexes. The first index is
-- the \"/from/\" location, the second is the \"/to/\" location.
fetch :: LookupTable -> Int -> Int -> Int
fetch (LookupTable arr) s1 s2 = arr ! (s1 - s2)

-- | Returns a list of representative 'Square' pairs that cover all the lookup
-- table index values. On a square board, there are @(2x-1)^2@ possible index
-- values and @x^4@ different 'Square' pairs, so the representative set is much
-- smaller than iterating over all possible pair combinations when creating
-- lookup tables.
--
-- This function is very expensive on larger board sizes: we compute all
-- candidates before filtering out duplicates in the current implementation.
-- However, it is typically only executed once and then the results reused, so
-- the cost is a negligible contribution to the total runtime in practice.
repIndexList :: BoardSize -> CoveringIndexList
repIndexList s@(BoardSize h v _) = CL $ map head $ groupBy compFirst $ sort l
    where l = [(d1 - d2, (s1, s2)) | (d1, s1) <- squares, (d2, s2) <- squares]
          compFirst x y = fst x == fst y
          squares = zip (map (squareToIndex s) sqs) sqs
          sqs = [Square (x, y) | x <- [0 .. h - 1], y <- [0 .. v - 1]]


-- $tables
-- Some useful lookup arrays that are commonly required across most chess
-- variants (more specific tables are created in the particular variant
-- files).  Each of these take a 'CoveringIndexList' parameter so that it only
-- has to be generated once (see documentation of 'repIndexList' for the
-- algorithmic complexity description).

-- | File separation between two squares.
fileTable :: CoveringIndexList -> LookupTable
fileTable cl = distanceTableWith f cl
    where f (Square s1) (Square s2) = abs (fst s1 - fst s2)

-- | Rank separation between two squares.
rankTable :: CoveringIndexList -> LookupTable
rankTable cl = distanceTableWith f cl
    where f (Square s1) (Square s2) = abs (snd s1 - snd s2)

-- | Square diagonal distance between two squares (this is using the chessboard
-- metric, not the Euclidean one).
squareTable :: CoveringIndexList -> LookupTable
squareTable cl = distanceTableWith f cl
    where f (Square s1) (Square s2) =
            max (abs (fst s1 - fst s2)) (abs (snd s1 - snd s2))

