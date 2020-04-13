import qualified Data.Vector.Unboxed as VU

-- nCkを計算
-- フェルマーの小定理を利用

-- 繰り返し自乗法
-- n の k 乗を m で割った余り
powMod :: Int -> Int -> Int -> Int
powMod n k m
    | k == 0 = 1
    | odd k = (powMod n (k-1) m) * n `mod` m
    | otherwise = t * t `mod` m
        where
            t = powMod n (k `div` 2) m

-- フェルマーの小定理を利用し逆元を求める
-- a^(m-2) ≡ a^(-1) (mod m)
-- a の (m - 2) 乗を m で割った余り
invModWithFermat :: Int -> Int -> Int
invModWithFermat a m = powMod a (m - 2) m

-- nCk = n * (n-1) * (n-2) .. * (n-k+1) / k! 
combinationV :: Int -> Int -> Int -> Int
combinationV n k m  = numerator * denominator `mod` m
    where
        numerator = VU.foldl' (\acc x -> acc * x `mod` m) 1 $ VU.fromList [n, n - 1, n - k + 1]
        denominator = invModWithFermat (VU.foldl' (\acc x -> acc * x `mod` m) 1 $ VU.fromList [1..k]) m

-- nCk = n * (n-1) * (n-2) .. * (n-k+1) / k! 
combination :: Int -> Int -> Int -> Int
combination n k m  = numerator * denominator `mod` m
    where
        numerator = foldl' (\acc x -> acc * x `mod` m) 1 [n, n - 1, n - k + 1]
        denominator = invModWithFermat (foldl' (\acc x -> acc * x `mod` m) 1 [1..k]) m

-- 使用例
-- main = do
--     n <- readLn :: IO Int
--     k <- readLn :: IO Int
--     let m = 10 ^ 9 + 7
--     print $ combination n k m