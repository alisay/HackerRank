import Data.Array ( Ix, accumArray, assocs )

countingSort :: (Ix i, Num a) => [i] -> i -> i -> [a]
countingSort l lo hi = [ times | (n, times) <- counts]
  where counts = assocs (accumArray (+) 0 (lo, hi) [(i, 1) | i <- l])

