--{-# LANGUAGE BangPatterns #-}
--{-# LANGUAGE ScopedTypeVariables #-}
--{-# LANGUAGE FlexibleContexts #-}
--{-# LANGUAGE ViewPatterns #-}
--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE Strict #-}
--{-# LANGUAGE NumericUnderscores #-}
--{-# LANGUAGE BlockArguments #-}
--{-# LANGUAGE MultiWayIf #-}

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as MPS
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
import Control.Monad.State.Strict
import Debug.Trace
-- import qualified Data.Vector.Algorithms.Merge as VAM
-- import qualified Data.Heap as HP
-- import Numeric.Extra
-- import Data.Tuple.Extra
-- import Data.List.Extra

--------------------------------------------------------------------------
main = do
    putStrLn "Hello, world!"
--------------------------------------------------------------------------

-- Input Util
int :: IO Int
int = BC.getLine >>= return . bsToInt

double :: IO Double
double = BC.getLine >>= return . bsToDouble

str :: IO String
str = BC.getLine >>= return . BC.unpack

strBS :: IO BC.ByteString
strBS = BC.getLine

-- convert
bsToInt :: BC.ByteString -> Int
bsToInt = fst . fromJust . BC.readInt

bsToDouble :: BC.ByteString -> Double
bsToDouble = read . BC.unpack

bsToIntTuple :: BC.ByteString -> (Int, Int)
bsToIntTuple bs = (a,b)
    where
        Just (a,bs') = parseInt bs
        Just (b,_) = parseInt bs'

bsToIntTriple :: BC.ByteString -> (Int, Int, Int)
bsToIntTriple bs = (a,b,c)
    where
        Just (a,bs') = parseInt bs
        Just (b,bs'') = parseInt bs'
        Just (c,_) = parseInt bs''

parseInt :: BC.ByteString -> Maybe (Int, BC.ByteString)
parseInt = BC.readInt . BC.dropWhile isSpace

-- single line
fromSLineL :: (BC.ByteString -> a) -> IO [a]
fromSLineL f = strBS >>= return . map f . BC.words

-- from single line to List / VU.Vector 
sLineToIntL :: IO [Int]
sLineToIntL = fromSLineL bsToInt

sLineToDoubleL :: IO [Double]
sLineToDoubleL = fromSLineL bsToDouble

sLineToStrL :: IO [String]
sLineToStrL = fromSLineL BC.unpack

sLineToStrBSL :: IO [BC.ByteString]
sLineToStrBSL = fromSLineL id 

sLineToIntV :: Int -> IO (VU.Vector Int)
sLineToIntV n = BC.getLine >>= return . VU.unfoldrN n parseInt

sLineToDoubleV :: IO (VU.Vector Double)
sLineToDoubleV = BC.getLine >>= return . VU.fromList . map bsToDouble . BC.words

-- multiple lines
fromMLinesL :: Int -> (BC.ByteString -> a) -> IO [a]
fromMLinesL n f = replicateM n (strBS >>= return . f)

fromMLinesV :: VU.Unbox a => Int -> (BC.ByteString -> a) -> IO (VU.Vector a)
fromMLinesV n f = VU.replicateM n (strBS >>= return . f)

-- from multiple lines to List / VU.Vector
mLinesToIntL :: Int -> IO [Int]
mLinesToIntL n = fromMLinesL n bsToInt

mLinesToDoubleL :: Int -> IO [Double]
mLinesToDoubleL n = fromMLinesL n bsToDouble

mLinesToStrL :: Int -> IO [String]
mLinesToStrL n = fromMLinesL n BC.unpack

mLinesToStrBSL :: Int -> IO [BC.ByteString]
mLinesToStrBSL n = fromMLinesL n id

mLinesToTupleL :: Int -> IO [(Int, Int)]
mLinesToTupleL n = fromMLinesL n bsToIntTuple

mLinesToTripleL :: Int -> IO [(Int, Int, Int)]
mLinesToTripleL n = fromMLinesL n bsToIntTriple

mLinesToIntV :: Int -> IO (VU.Vector Int)
mLinesToIntV n = fromMLinesV n bsToInt

mLinesToDoubleV :: Int -> IO (VU.Vector Double)
mLinesToDoubleV n = fromMLinesV n bsToDouble

mLinesToTupleV :: Int -> IO (VU.Vector (Int, Int))
mLinesToTupleV n = fromMLinesV n bsToIntTuple
    
mLinesToTripleV :: Int -> IO (VU.Vector (Int, Int, Int))
mLinesToTripleV n = fromMLinesV n bsToIntTriple

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
