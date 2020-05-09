import qualified Data.Map as MP

-- 素因数分解
primeFactor :: MP.Map Int Int -> Int -> MP.Map Int Int
primeFactor m n = loop n 2 m
    where
        loop :: Int -> Int -> MP.Map Int Int ->  MP.Map Int Int
        loop n i m
            | n `rem` i == 0 = inner n i m
            | i * i > n && n /= 1 = add n m
            | i * i > n && n == 1 = m
            | otherwise = loop n (i + 1) m

        inner :: Int -> Int -> MP.Map Int Int -> MP.Map Int Int
        inner n i m
            | n `rem` i == 0 = inner (n `quot` i) i m'
            | otherwise = loop n (i + 1) m
            where
                m' = add i m

        add :: Int -> MP.Map Int Int -> MP.Map Int Int 
        add k m = MP.insertWith (+) k 1 m       