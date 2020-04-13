import Data.Bits
import qualified Data.Vector.Unboxed as VU

toBit :: Int -> Int -> [Int]
toBit n l 
    | l == 0 = [] 
    | otherwise = toBit (shiftR n 1) (l - 1) ++ [n .&. 1]

toBitV :: Int -> Int -> VU.Vector Int
toBitV n l
    | l == 0 = VU.empty 
    | otherwise = VU.snoc (toBitV (shiftR n 1) (l - 1)) (n .&. 1)