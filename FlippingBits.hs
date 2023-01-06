import Data.Bits (Bits(xor))

flippingBits :: Integer -> Integer
flippingBits = 
    toDec . flipBits . bitStuff . toBin

toBin :: Integer -> [Integer]
toBin = reverse . toBin' where
    toBin' 0 = []
    toBin' n = (n `mod` 2) : toBin' (n `div` 2)

toDec :: [Integer] -> Integer
toDec = foldl (\acc x -> (acc * 2) + x) 0

bitStuff ::  [Integer] -> [Integer]
bitStuff xs = take (32 - length xs) [0,0..] ++ xs

flipBits :: [Integer] -> [Integer]
flipBits [] = []
flipBits xs = map (`xor` 1) xs
