-- 約数列挙
divisor :: Int -> [Int]
divisor n = loop n 1
    where
        loop :: Int -> Int -> [Int]
        loop n i
            | n < i * i = []
            | n `mod` i == 0 && (i * i == n) = i : loop n (i+1)
            | n `mod` i == 0 && (i * i /= n) = i : (n `div` i) : loop n (i+1)
            | otherwise = loop n (i + 1)
