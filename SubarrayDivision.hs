import Data.List

birthday' :: (Eq a, Num a) => [a] -> a -> Int -> Int
birthday' s d m = length $ (filter (\x -> length x == m && sum x == d) . concatMap tails . inits) s
