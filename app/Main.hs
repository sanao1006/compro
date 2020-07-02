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
import qualified Data.OrdPSQ as PSQ
import qualified Data.Vector.Algorithms.Merge as VAM
import qualified Data.Heap as HP
import Numeric.Extra
import Data.Tuple.Extra
import Data.List.Extra

data OrdQ = Ascending | Descending

--------------------------------------------------------------------------
main = do
    putStrLn "Hello, world!"
--------------------------------------------------------------------------

-- Input Util
int :: IO Int
int = bsToInt <$> BC.getLine

double :: IO Double
double = bsToDouble <$> BC.getLine

str :: IO String
str = BC.unpack <$> BC.getLine

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
fromSLineL f =  map f . BC.words <$> strBS

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
sLineToIntV n = VU.unfoldrN n parseInt <$> BC.getLine

sLineToDoubleV :: IO (VU.Vector Double)
sLineToDoubleV = VU.fromList . map bsToDouble . BC.words <$> BC.getLine

-- multiple lines
fromMLinesL :: Int -> (BC.ByteString -> a) -> IO [a]
fromMLinesL n f = replicateM n (f <$> strBS)

fromMLinesV :: VU.Unbox a => Int -> (BC.ByteString -> a) -> IO (VU.Vector a)
fromMLinesV n f = VU.replicateM n (f <$> strBS)

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
m1 = subtract 1

toDouble :: Int -> Double
toDouble = fromIntegral

toInt :: Double -> Int
toInt = floor

sqToList :: SQ.Seq a -> [a]
sqToList sq =
    case SQ.viewl sq of
        l SQ.:< sq' -> l : sqToList sq'
        SQ.EmptyL -> []

sortV :: (Ord a, VU.Unbox a) => VU.Vector a -> VU.Vector a
sortV v = VU.create $ do
    w <- VU.thaw v
    VAM.sort w
    return w

buildPQL :: Ord a => OrdQ -> [a] -> PSQ.OrdPSQ Int Int a
buildPQL oq xs =
    case oq of
        Ascending -> f $ zip3 [1..l] [1..l] $ sort xs
        Descending -> f $ zip3  [1..l] (map negate [1..l]) $ sort xs
    where
        l = length xs
        f = foldl' g PSQ.empty
        g acc (k,p,v) = PSQ.insert k p v acc

buildPQV :: (Ord a, VU.Unbox a) => OrdQ -> VU.Vector a -> PSQ.OrdPSQ Int Int a
buildPQV oq xs =
    case oq of
        Ascending -> f $ VU.zip3 (VU.generate l (+1)) (VU.generate l (+1)) $ sortV xs
        Descending -> f $ VU.zip3  (VU.generate l (+1)) (VU.generate l $ negate . (+1)) $ sortV xs
    where
        l = VU.length xs
        f = VU.foldl' g PSQ.empty
        g acc (k,p,v) = PSQ.insert k p v acc