-- 素数判定
isPrime :: Int -> Bool
isPrime n = loop n 2
    where
        loop :: Int -> Int -> Bool
        loop n i
            | n <= 1 = False
            | n < i * i = True
            | n `rem` i == 0 = False
            | otherwise = loop n (i + 1)