--{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as MP
import qualified Data.Set as ST
import qualified Data.Sequence as SQ
import Data.Array
import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.IO
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.Char
import Data.Bits
import Data.List
import Data.Tuple
import Data.Functor
import Data.IORef
import Data.STRef
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Debug.Trace
--import qualified Data.Vector.Algorithms.Merge as VAM
--import qualified Data.Heap as HP
--import Numeric.Extra
--import Data.Tuple.Extra
--import Data.List.Extra

--------------------------------------------------------------------------
main = do
    n <- int
    xs <- sLineToIntV n
    print xs
--------------------------------------------------------------------------

-- Input Util
int :: IO Int
int = BC.getLine >>= return . bsToInt

double :: IO Double
double = BC.getLine >>= return . bsToDouble

str :: IO String
str = getLine

strBS :: IO BC.ByteString
strBS = BC.getLine

bsToInt :: BC.ByteString -> Int
bsToInt = fst . fromJust . BC.readInt

bsToDouble :: BC.ByteString -> Double
bsToDouble = read . BC.unpack

parseInt :: BC.ByteString -> Maybe (Int, BC.ByteString)
parseInt = BC.readInt . BC.dropWhile isSpace

sLineToIntL :: IO [Int]
sLineToIntL = strBS >>= return . map bsToInt . BC.words

sLineToDoubleL :: IO [Double]
sLineToDoubleL = strBS >>= return . map bsToDouble . BC.words

sLineToIntV :: Int -> IO (VU.Vector Int)
sLineToIntV n = strBS >>= return . VU.unfoldrN n parseInt

sLineToDoubleV :: IO (VU.Vector Double)
sLineToDoubleV = strBS >>= return . VU.fromList . map bsToDouble . BC.words

mLinesToIntL :: Int -> IO [Int]
mLinesToIntL n = replicateM n (strBS >>= return . bsToInt)

mLinesToDoubleL :: Int -> IO [Double]
mLinesToDoubleL n = replicateM n (strBS >>= return . bsToDouble)

mLinesToIntV :: Int -> IO (VU.Vector Int)
mLinesToIntV n = VU.replicateM n (strBS >>= return . bsToInt)

mLinesToDoubleV :: Int -> IO (VU.Vector Double)
mLinesToDoubleV n = VU.replicateM n (strBS >>= return . bsToDouble)

mLinesToTupleL :: Int -> IO [(Int, Int)]
mLinesToTupleL n = replicateM n (strBS >>= return . (\[a,b] -> (a,b)) . map bsToInt . BC.words)

mLinesToTripleL :: Int -> IO [(Int, Int, Int)]
mLinesToTripleL n = replicateM n (strBS >>= return . (\[a,b,c] -> (a,b,c)) . map bsToInt . BC.words)

mLinesToTupleV :: Int -> IO (VU.Vector (Int, Int))
mLinesToTupleV n = VU.replicateM n $ do
    bs <- strBS
    let
        Just (a, bs') = parseInt bs
        Just (b, _) = parseInt bs'
    return (a,b)
    
mLinesToTripleV :: Int -> IO (VU.Vector (Int, Int, Int))
mLinesToTripleV n = VU.replicateM n $ do
    bs <- strBS
    let
        Just (a, bs') = parseInt bs
        Just (b, bs'') = parseInt bs'
        Just (c, _) = parseInt bs''
    return (a,b,c)

-- output
yesnoL :: Bool -> IO ()
yesnoL cond
    | cond = putStrLn "Yes"
    | otherwise = putStrLn "No"

yesnoU :: Bool -> IO ()
yesnoU cond
    | cond = putStrLn "YES"
    | otherwise = putStrLn "NO"

-- others
strToChr :: String -> Char
strToChr [x] = x

chrToStr :: Char -> String
chrToStr x = [x]

m1 :: Int -> Int
m1 x = subtract 1 x

toDouble :: Int -> Double
toDouble x = fromIntegral x

toInt :: Double -> Int
toInt x = floor x
