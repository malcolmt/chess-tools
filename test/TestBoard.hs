module TestBoard
where

import Control.Applicative ((<$>), (<*>))
import Data.List (group, sort)
import Test.QuickCheck

import ChessTools.Board
import ChessTools.Board.Internal


instance Show BoardSize where
    show (BoardSize h v vbuf) =
            "BoardSize " ++ unwords (map show [h, v, vbuf])

instance Arbitrary BoardSize where
    arbitrary = sized $ \n -> do
        let n' = n + 2
        dx <- choose (2, n')
        dy <- choose (2, n')
        vbuf <- choose (0, 4)
        return $ BoardSize dx dy vbuf


-- | For some of the more complex tests (there's at least one function that is
-- O(n^4), for example), it's more feasible to only generate small realistic
-- board sizes. An upper bound of 11 by 11 is arbitrarily used here.
smallBoardGen :: Gen BoardSize
smallBoardGen = sized $ \n ->
    resize (min n 11) arbitrary


-- | Square coordinates depend upon the dimensions of the board that contains
-- them. Hence, this generator requires a seeding parameter: the board size.
genSquare :: BoardSize -> Gen Square
genSquare bs = do
    sx <- choose (0, boardHorizSize bs - 1)
    sy <- choose (0, boardVertSize bs - 1)
    return $ Square (sx, sy)

genTwoSquares :: BoardSize -> Gen (Square, Square)
genTwoSquares bs = (,) <$> genSquare bs <*> genSquare bs

boardAndSquareGen :: Gen (BoardSize, Square)
boardAndSquareGen = do
    bs <- arbitrary :: Gen BoardSize
    sq <- genSquare bs
    return (bs, sq)

boardAndTwoSquareGen :: Gen (BoardSize, Square, Square)
boardAndTwoSquareGen = do
    bs <- arbitrary :: Gen BoardSize
    s1 <- genSquare bs
    s2 <- genSquare bs
    return (bs, s1, s2)


-- XXX: It's a little annoying that this is precisely how squareToIndex is
-- implemented, so it's not really verifying the result of that conversion by
-- different means.

boardAndIndexGen :: Gen (BoardSize, Int)
boardAndIndexGen = do
    bs <- arbitrary :: Gen BoardSize
    Square (dx, dy) <- genSquare bs
    return (bs, (dy + boardVertBuffer bs) * rowLength bs + dx + leftBuf bs)


-- The squareToIndex and indexToSquare functions should be inverses of each
-- other. That is:
--      index -> square -> index should be the identity
--      square -> index -> square should be the identity
prop_index_to_square_inverse = forAll boardAndIndexGen $ \(b, idx) ->
    squareToIndex b (indexToSquare b idx) == idx

prop_square_to_index_inverse = forAll boardAndSquareGen $ \(b, sq) ->
    indexToSquare b (squareToIndex b sq) == sq

-- As squares move from lower left ("a1" in western chess) to upper right (h8),
-- the index into the lookup table should increase.
prop_index_increases_with_square = forAll boardAndTwoSquareGen $ \(b, s1, s2) ->
    let Square (c1, r1) = s1
        Square (c2, r2) = s2
        idx1 = squareToIndex b s1
        idx2 = squareToIndex b s2
    in case r1 `compare` r2 of
        GT -> idx1 > idx2
        LT -> idx1 < idx2
        EQ -> case c1 `compare` c2 of
                GT -> idx1 > idx2
                LT -> idx1 < idx2
                EQ -> True

-- The board array size should be computed correctly (this is the
-- representation of the board of pieces, not a lookup array, which is smaller).
prop_board_array_size bs = boardArraySize bs == expected
    where BoardSize h v vbuf = bs
          expected = h * v + v * (h - 1) + 2 * vbuf * (2 * h - 1)

-- For any two squares s1 and s2, the value of constOffset + s1 - s2 should be
-- between 0 and the lookup array maximum size.
prop_lookup_index_is_positive = forAll boardAndTwoSquareGen $ \(b, s1, s2) ->
    let idx1 = squareToIndex b s1
        idx2 = squareToIndex b s2
        offset = constOffset b + idx1 - idx2
    in offset >= 0 && offset < 2 * offset + 1


-- The list returned from repIndexList should actually be representative. That
-- is, it should contain as many values as the size of the lookup array and all
-- of the distance values in it should be unique.
prop_repIndexList_is_representative = forAll smallBoardGen $ \bs ->
    let CL xs = repIndexList bs
    in length xs == 2 * constOffset bs + 1 &&
       (length . group . sort $ map fst xs) == length xs


-- Check that file, rank and square distance lookups are evaluated correctly.
-- For speed purposes, we check each of these against two fixed board sizes
-- (one square and one not). This avoids having to continually regenerate the
-- representative index list.

board1 = BoardSize 8 8 2
board2 = BoardSize 8 9 2
repList1 = repIndexList board1
repList2 = repIndexList board2
fTable1 = fileTable board1 repList1
fTable2 = fileTable board2 repList2
rTable1 = rankTable board1 repList1
rTable2 = rankTable board2 repList2
sTable1 = squareTable board1 repList1
sTable2 = squareTable board2 repList2

fileCheckFunc (Square s1) (Square s2) = abs $ fst s1 - fst s2
rankCheckFunc (Square s1) (Square s2) = abs $ snd s1 - snd s2
squareCheckFunc sq1 sq2 = max (fileCheckFunc sq1 sq2) (rankCheckFunc sq1 sq2)

checkLookup lt cmp b = forAll (genTwoSquares b) $ \(sq1, sq2) ->
    let idx1 = squareToIndex b sq1
        idx2 = squareToIndex b sq2
    in fetch lt idx1 idx2 == cmp sq1 sq2

prop_check_file_distance_1 = checkLookup fTable1 fileCheckFunc board1
prop_check_file_distance_2 = checkLookup fTable2 fileCheckFunc board2
prop_check_rank_distance_1 = checkLookup rTable1 rankCheckFunc board1
prop_check_rank_distance_2 = checkLookup rTable2 rankCheckFunc board2
prop_check_square_distance_1 = checkLookup sTable1 squareCheckFunc board1
prop_check_square_distance_2 = checkLookup sTable2 squareCheckFunc board2

