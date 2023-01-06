import Data.List ( group, sort )

lonelyinteger :: [Int] -> Int
lonelyinteger = head . head .filter ((<2). length) . group . sort