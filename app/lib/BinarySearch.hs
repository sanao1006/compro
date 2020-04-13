-- 二分探索
-- めぐる式二分探索
-- target以上の最小の値を求める
-- [from..to]のListで考えると、返ってくるのはtarget以上を満たす最小のインデックスになる
-- fromを求めたいなら to - 1 をする必要あり

binarySearch :: (Int, Int) -> Int -> Int
binarySearch (from, to) target
    | to - from <= 1 = to
    | mid > target = binarySearch (from, mid) target
    | mid <= target = binarySearch (mid, to) target 
        where
            mid = (from + to) `div` 2

binarySearchV :: VU.Vector Int -> Int -> Int
binarySearchV vec k = loop left right vec k
  where
    left = -1
    right = VU.length vec
    loop l r xs a
      | r - l <= 1 = r 
      | isok = loop l mid xs a
      | otherwise = loop mid r xs a
        where
          mid = l + (r - l) `div` 2
          isok = (xs VU.! mid) >= a

binarySearchA :: Array Int Int -> Int -> Int
binarySearchA arr k = loop left right arr k
  where
    left = -1
    right = length arr
    loop l r xs a
      | r - l <= 1 = r 
      | isok = loop l mid xs a
      | otherwise = loop mid r xs a
        where
          mid = l + (r - l) `div` 2
          isok = (xs ! mid) >= a
