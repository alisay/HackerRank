timeConversion :: String -> String
timeConversion s = do
    timeConversion' s

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