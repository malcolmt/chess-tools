{- Some QuickCheck generators and other utility functions used in a few test
 - modules.
 -}

module ChessTools.Test.Utils (
      boardSizeGen
    , genIndex
    , genBadIndex
    , genSquare
) where

import Test.QuickCheck

import ChessTools.Board
import ChessTools.Board.Internal


-- This could be an Arbitrary instance, but it would be an orphan. We don't
-- want to put the instance in ChessTools.Board.Internal to avoid unnecessary
-- QuickCheck dependencies, so prefer to use a direct Gen function here.
boardSizeGen :: Gen BoardSize
boardSizeGen = sized $ \n -> do
    let n' = n + 2
    dx <- choose (2, n')
    dy <- choose (2, n')
    vbuf <- choose (0, 4)
    return $ BoardSize dx dy vbuf

genIndex :: BoardSize -> Gen BIndex
genIndex bs = do
    Square (dx, dy) <- genSquare bs
    return $ BI ((dy + boardVertBuffer bs) * rowLength bs + dx + leftBuf bs)

genBadIndex :: BoardSize -> Gen BIndex
genBadIndex (BoardSize h v vbuf)
    | vbuf == 0 = leftBuffer
    | otherwise = oneof [leftBuffer, above, below]
    where rl = 2 * h - 1
          leftBuffer = do
            x <- choose (0, v - 1)
            y <- choose (0, h `div` 2 - 1)
            return $ BI ((vbuf + x) * rl + y)

          above = do
            x <- choose (0, vbuf - 1)
            y <- choose (0, rl - 1)
            return $ BI ((vbuf + v + x) * rl + y)

          below = do
            x <- choose (0, vbuf - 1)
            y <- choose (0, rl - 1)
            return $ BI (x * rl + y)

genSquare :: BoardSize -> Gen Square
genSquare bs = do
    sx <- choose (0, boardHorizSize bs - 1)
    sy <- choose (0, boardVertSize bs - 1)
    return $ Square (sx, sy)

