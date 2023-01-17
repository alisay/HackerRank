import Data.Char (toLower)

pangrams :: String -> String
pangrams s 
    | all (`elem` map toLower s) ['a'..'z']  = "pangram"
    | otherwise = "not pangram"