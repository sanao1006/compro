-- 繰り返し自乗法
-- n の k 乗を m で割った値
powMod :: Int -> Int -> Int -> Int
powMod n k m
    | k == 0 = 1
    | odd k = (powMod n (k - 1) m) * n `mod` m
    | otherwise = t * t `mod` m
        where
            t = powMod n (k `div` 2) m

-- 拡張ユークリッドの互除法
-- a b は s * a + t * b == g となる (g, s, t) を返す
exEuclid :: Int -> Int -> (Int, Int, Int)
exEuclid a b = loop 1 0 0 1 a b
    where
        loop s t s' t' a b
            | b == 0 = (a, s, t)
            | otherwise = case divMod a b of
                (q, r) -> loop s' t' (s - q * s') (t - q * t') b r

-- 逆元
-- 拡張ユークリッドの互除法から
invModWithEuclid :: Int -> Int -> Int
invModWithEuclid a m =
    case exEuclid a m of
        (1, s, t) -> s `mod` m
        (-1, s, t) -> (negate s) `mod` m
        _ -> error $ show a ++ " has no inverse modulo " ++ show m

-- 逆元
-- フェルマーの小定理から
-- a^(m-2) ≡ a^(-1) (mod m)
invModWithFermat :: Int -> Int -> Int
invModWithFermat a m = powMod a (m - 2)
        