module ChessTools.Test.Board
where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (join, liftM)
import Data.List (group, sort)
import Data.Maybe (fromJust)
import Test.QuickCheck

import ChessTools.Board
import ChessTools.Board.Internal
import ChessTools.Test.Utils


-- | For some of the more complex tests (there's at least one function that is
-- O(n^4), for example), it's more feasible to only generate small realistic
-- board sizes. An upper bound of 11 by 11 is arbitrarily used here.
smallBoardGen :: Gen BoardSize
smallBoardGen = sized $ \n ->
    resize (min n 11) boardSizeGen

genBadSquare :: BoardSize -> Gen Square
genBadSquare (BoardSize h v _) = oneof [badX, badY]
    where badX = do
            sx <- oneof [choose (-5, -1), choose (h, h + 5)]
            sy <- choose (-5, v + 5)
            return $ Square (sx, sy)

          badY = do
            sx <- choose (-5, h + 5)
            sy <- oneof [choose (-5, -1), choose (v, v + 5)]
            return $ Square (sx, sy)

genTwoSquares :: BoardSize -> Gen (Square, Square)
genTwoSquares bs = (,) <$> genSquare bs <*> genSquare bs

boardAndSquareGen :: Gen (BoardSize, Square)
boardAndSquareGen = do
    bs <- boardSizeGen
    sq <- genSquare bs
    return (bs, sq)

boardAndTwoSquareGen :: Gen (BoardSize, Square, Square)
boardAndTwoSquareGen = do
    bs <- boardSizeGen
    s1 <- genSquare bs
    s2 <- genSquare bs
    return (bs, s1, s2)

boardAndBadSquareGen :: Gen (BoardSize, Square)
boardAndBadSquareGen = do
    bs <- boardSizeGen
    sq <- genBadSquare bs
    return (bs, sq)

-- XXX: It's a little annoying that this is precisely how squareToIndex is
-- implemented, so it's not really verifying the result of that conversion by
-- different means.

boardAndIndexGen :: Gen (BoardSize, BIndex)
boardAndIndexGen = do
    bs <- boardSizeGen
    idx <- genIndex bs
    return (bs, idx)

boardAndBadIndexGen :: Gen (BoardSize, BIndex)
boardAndBadIndexGen = do
    bs <- boardSizeGen
    idx <- genBadIndex bs
    return (bs, idx)

-- The squareToIndex and indexToSquare functions should be inverses of each
-- other. That is:
--      index -> square -> index should be the identity
--      square -> index -> square should be the identity
prop_indexToSquareInverse :: Property
prop_indexToSquareInverse = forAll boardAndIndexGen $ \(b, idx) ->
    join (squareToIndex b `liftM` indexToSquare b idx) == Just idx

prop_squareToIndexInverse :: Property
prop_squareToIndexInverse = forAll boardAndSquareGen $ \(b, sq) ->
    join (indexToSquare b `liftM` squareToIndex b sq) == Just sq

-- squareToIndex and indexToSquare should handle bad input appropriately.
prop_errorSquareToIndex :: Property
prop_errorSquareToIndex = forAll boardAndBadSquareGen $ \(b, sq) ->
    squareToIndex b sq == Nothing

prop_errorIndexToSquare :: Property
prop_errorIndexToSquare = forAll boardAndBadIndexGen $ \(b, idx) ->
    indexToSquare b idx == Nothing

-- As squares move from lower left ("a1" in western chess) to upper right (h8),
-- the index into the lookup table should increase.
prop_indexIncreasesWithSquare :: Property
prop_indexIncreasesWithSquare = forAll boardAndTwoSquareGen $ \(b, s1, s2) ->
    let idx1 = squareToIndex b s1
        idx2 = squareToIndex b s2
    in s1 `compare` s2 == idx1 `compare` idx2

-- The board array size should be computed correctly (this is the
-- representation of the board of pieces, not a lookup array, which is smaller).
prop_boardArraySize :: Property
prop_boardArraySize = forAll boardSizeGen $ \b ->
    let BoardSize h v vbuf = b
        expected = h * v + v * (h - 1) + 2 * vbuf * (2 * h - 1)
    in boardArraySize b == expected

-- The list returned from repIndexList should actually be representative. That
-- is, it should contain as many values as the size of the lookup array and all
-- of the distance values in it should be unique.
prop_repIndexListRepresents :: Property
prop_repIndexListRepresents = forAll smallBoardGen $ \bs ->
    let cl@(CL xs) = repIndexList bs
        (l, u) = lookupBounds cl
    in length xs == fromLI u - fromLI l + 1 &&
       (length . group . sort $ map fst xs) == length xs


-- Check that file, rank and square distance lookups are evaluated correctly.
-- For speed purposes, we check each of these against two fixed board sizes
-- (one square and one not). This avoids having to continually regenerate the
-- representative index list.

board1, board2 :: BoardSize
board1 = BoardSize 8 8 2
board2 = BoardSize 8 9 2

repList1, repList2 :: CoveringIndexList
repList1 = repIndexList board1
repList2 = repIndexList board2

fTable1, fTable2, rTable1, rTable2, sTable1, sTable2 :: LookupTable
fTable1 = fileTable repList1
fTable2 = fileTable repList2
rTable1 = rankTable repList1
rTable2 = rankTable repList2
sTable1 = squareTable repList1
sTable2 = squareTable repList2

type SquareCmpFunc = Square -> Square -> Int

fileCheckFunc, rankCheckFunc, squareCheckFunc :: SquareCmpFunc
fileCheckFunc (Square s1) (Square s2) = abs $ fst s1 - fst s2
rankCheckFunc (Square s1) (Square s2) = abs $ snd s1 - snd s2
squareCheckFunc sq1 sq2 = max (fileCheckFunc sq1 sq2) (rankCheckFunc sq1 sq2)

checkLookup :: LookupTable -> SquareCmpFunc -> BoardSize -> Property
checkLookup lt cmp b = forAll (genTwoSquares b) $ \(sq1, sq2) ->
    let idx1 = fromJust $ squareToIndex b sq1
        idx2 = fromJust $ squareToIndex b sq2
    in fetch lt idx1 idx2 == cmp sq1 sq2

prop_checkFileDistance1 :: Property
prop_checkFileDistance1 = checkLookup fTable1 fileCheckFunc board1

prop_checkFileDistance2 :: Property
prop_checkFileDistance2 = checkLookup fTable2 fileCheckFunc board2

prop_checkRankDistance1 :: Property
prop_checkRankDistance1 = checkLookup rTable1 rankCheckFunc board1

prop_checkRankDistance2 :: Property
prop_checkRankDistance2 = checkLookup rTable2 rankCheckFunc board2

prop_checkSquareDistance1 :: Property
prop_checkSquareDistance1 = checkLookup sTable1 squareCheckFunc board1

prop_checkSquareDistance2 :: Property
prop_checkSquareDistance2 = checkLookup sTable2 squareCheckFunc board2

