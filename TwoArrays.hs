import Data.List(sort)

twoArrays k a b = do 
    output $ twoArrays' k a b

twoArrays' :: (Num t, Ord t) => t -> [t] -> [t] -> Bool
twoArrays' k xs ys = 
    compare k zipped where
        zipped = zipWith (+) ((reverse . sort) xs) (sort ys) 
        compare k (x:xs) = x >= k && compare k xs
        compare _ [] = True

output :: Bool -> [Char]
output True = "YES"
output _ = "NO"