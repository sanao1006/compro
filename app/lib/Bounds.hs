import qualified Data.Vector.Unboxed as VU

-- 下限境界 (Vector 付き)
lowerBoundV :: VU.Vector Int -> (Int, Int) -> Int -> Int
lowerBoundV vec (from, to) target
    | from == to = from
    | mValue >= target = lowerBoundV vec (from, mid) target
    | mValue < target = lowerBoundV vec (mid + 1, to) target
        where
            mid = (from + to) `div` 2
            mValue = vec VU.! mid
 
-- 上限境界 (Vector 付き)
upperBoundV :: VU.Vector Int -> (Int, Int) -> Int -> Int
upperBoundV vec (from, to) target
    | from == to = from
    | mValue > target = upperBoundV vec (from, mid) target
    | mValue <= target = upperBoundV vec (mid + 1, to) target
        where
            mid = (from + to) `div` 2
            mValue = vec VU.! mid

-- 下限境界 (Array 付き)
lowerBoundA :: Array Int Int -> (Int, Int) -> Int -> Int
lowerBoundA arr (from, to) target
    | from == to = from
    | mValue >= target = lowerBoundA arr (from, mid) target
    | mValue < target = lowerBoundA arr (mid + 1, to) target
        where
            mid = (from + to) `div` 2
            mValue = arr ! mid

-- 上限境界 (Array 付き)
upperBoundA :: Array Int Int -> (Int, Int) -> Int -> Int
upperBoundA arr (from, to) target
    | from == to = from
    | mValue > target = upperBoundA arr (from, mid) target
    | mValue <= target = upperBoundA arr (mid + 1, to) target
        where
            mid = (from + to) `div` 2
            mValue = arr ! mid