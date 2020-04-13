import qualified Data.Map as MP

-- 素因数分解
primeFactor :: MP.Map Int Int -> Int -> MP.Map Int Int
primeFactor m n = loop n 2 m
    where
        loop :: Int -> Int -> MP.Map Int Int ->  MP.Map Int Int
        loop n i m
            | n `rem` i == 0 = inner n i m
            | i * i > n = if n /= 1 then search n m else m 
            | otherwise = loop n (i+1) m

        inner :: Int -> Int -> MP.Map Int Int -> MP.Map Int Int
        inner n i m
            | n `rem` i == 0 = let m' = search i m in inner (n `quot` i) i m'
            | otherwise = loop n (i+1) m
        
        search :: Int -> MP.Map Int Int -> MP.Map Int Int 
        search k m = case MP.lookup k m of
            Just _ -> MP.adjust (+1) k m
            Nothing -> MP.insert k 1 m