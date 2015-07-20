module Ant where

import Data.Array.Repa (Z(..),(:.)(..),(!))
import qualified Data.Array.Repa as R

{-
  Direction   0
            3   1
              2
-}

type Dir  = Int
type Ant  = (Int, Int)
type Grid = R.Array R.U R.DIM2 Int

-- When n = 1, turn right
-- When n = 3, turn left
turn :: Dir -> Int ->  Dir
turn d n = (d + n) `mod` 4

forward :: Ant -> Dir -> Ant
forward (a,b) d | d == 0 = (a, b+1)
                | d == 1 = (a+1, b)
                | d == 2 = (a, b-1)
                | d == 3 = (a-1, b)

tick :: Ant -> Grid -> IO Grid
tick ant world = R.computeP $ R.traverse world id (\_ sh@(Z :. a :. b) ->
  if ant == (a,b)
    then turn (world ! sh) 2 
    else world ! sh)

zeroGrid :: Int -> Int -> IO Grid
zeroGrid w h = return $ R.fromListUnboxed (Z :. w :. h) (replicate (w*h) 3) 
