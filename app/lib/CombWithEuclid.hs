import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Control.Monad

-- nCkを計算
-- 拡張ユークリッドの互除法を利用

type Fact = VU.Vector Int
type FactInv = VU.Vector Int

-- nCk を計算するための前処理
-- 参考リンク
-- よくやる二項係数 (nCk mod. p)、逆元 (a^-1 mod. p) の求め方 (https://drken1215.hatenablog.com/entry/2018/06/08/210000)
-- Python で二項係数 nCr を高速に計算したい (https://www.planeta.tokyo/entry/5195/)
initCombinationTable :: Int -> Int -> IO (Fact, FactInv) 
initCombinationTable n m = do
    fact <- VUM.replicate (n + 100) (0 :: Int)
    factInv <- VUM.replicate (n + 100) (0 :: Int)
    inv <- VUM.replicate (n + 100) (0 :: Int)

    forM_ [0, 1] $ \i -> do
        VUM.write fact i 1
        VUM.write factInv i 1
        VUM.write inv i i

    forM_ [2..n+1] $ \i -> do
        VUM.read fact (i - 1) >>= \r -> VUM.write fact i (r * i `mod` m)
        VUM.read inv (m `mod` i) >>= \r -> VUM.write inv i (m - (r * (m `div` i) `mod` m))
        VUM.read factInv (i - 1) >>= \rfi -> VUM.read inv i >>= \ri -> VUM.write factInv i (rfi * ri `mod` m)
    
    fact' <- VU.unsafeFreeze fact
    factInv' <- VU.unsafeFreeze factInv

    return (fact', factInv')

-- nCk = n! / r! * (n-r)!
combination :: Fact -> FactInv -> Int -> Int -> Int -> Int
combination fact factInv n k m
    | n < k = 0
    | n < 0 || k < 0 = 0
    | otherwise = numerator * denominator `mod` m
        where 
            numerator = fact VU.! n 
            denominator = (factInv VU.! k) * (factInv VU.! (n - k)) `rem` m

-- 使用例
-- main = do
--     n <- readLn :: IO Int
--     k <- readLn :: IO Int
--     let m = 10 ^ 9 + 7
--     (fact, factInv) <- initCombinationTable n m
--     print $ combination fact factInv n k m
