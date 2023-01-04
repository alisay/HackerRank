import Data.List 

miniMaxSum :: [Int] -> IO ()
miniMaxSum arr = do
    putStrLn (stringify $ miniMaxSum' arr)

stringify :: (Show a1, Show a2) => (a1, a2) -> [Char]
stringify (x, y) = show x ++ " " ++ show y

miniMaxSum' :: [Int] -> (Int,Int)
miniMaxSum' xs = (minimum, maximum) where
    total = sum xs
    sorted = sort xs
    minimum = total - last sorted
    maximum = total - head sorted
