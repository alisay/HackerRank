diagonalDifference :: Num a => [[a]] -> a
diagonalDifference arr = abs(sumPrimary arr - sumSecondary arr) where
    sumPrimary xs = sum (zipWith (!!) xs [0..])
    sumSecondary xs = sum (zipWith  (!!) xs [(length xs-1),(length xs -2)..])

