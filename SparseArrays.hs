matchingStrings :: [String] -> [String] -> [Int]
matchingStrings xs (y:ys) = countOccurrences y xs : matchingStrings xs ys
matchingStrings _ [] = []

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (==x)
