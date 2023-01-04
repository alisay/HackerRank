import Data.Char(digitToInt)
import Sound.Tidal.Context (xDefault)

timeConversion :: [Char] -> IO ()
timeConversion s = do
    putStrLn (scan s)
-- scanString :: String -> Int
-- scanString = go 0
--     where go a [] = a
--           go a (x:xs) | '0' <= x && x <= '9' = go (10*a+sc) xs
--                       | x == 'P' = a + 120000
--                       | otherwise = go a xs
--               where sc = fromEnum x - fromEnum '0'

scan :: [Char] -> [Char]
scan xs 
    | xs !! 8 == 'P' = convert (map digitToInt (take 2 xs)) ++ drop 2 xs
    | otherwise = xs where 
        convert [x,y] = concatMap show [x+1,y+2]
        convert _ = []

timeConversion' :: String -> String
timeConversion' [a,b,c,d,e,f,g,h,i,j] = stuff (hours ++ [c,d,e,f,g,h]) where
    hours = show (((read[a,b]::Int) `mod` 12) + convert i)
    convert 'P' = 12
    convert _ = 0
timeConversion' _ = []

stuff :: String -> String
stuff x 
    | length x == 8 = x
    | otherwise = '0':x