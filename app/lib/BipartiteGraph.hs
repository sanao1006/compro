{-# LANGUAGE ScopedTypeVariables #-}
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Map as MP
import Control.Applicative
import Control.Monad
import Control.Monad.ST
import Data.Char
import Data.Tuple

type Graph = MP.Map Int (V.Vector Int)

make :: Int -> VU.Vector (Int, Int) -> Graph
make n xys = graph
    where
        init = VU.foldl' (\g i -> MP.insert i V.empty g) MP.empty $ VU.fromList [0..n-1] 
        graph = VU.foldl' (\g (f, t) -> MP.adjust (V.cons t) f g) init (xys VU.++ VU.map swap xys)

isBipartiteGraph :: Graph -> Int -> Bool
isBipartiteGraph graph n = 
    runST $ do
        (colors :: VUM.STVector s Int) <- VUM.replicate n 0
        bs <- forM [0..n-1] $ \i -> do
            c <- VUM.read colors i
            if c == 0
                then dfs graph colors i 1
                else return True
        return $ and bs
    where
        dfs :: Graph -> (VUM.STVector s Int) -> Int -> Int -> ST s Bool
        dfs graph colors i c = do
            ci <- VUM.read colors i
            if (ci == 0)
                then do
                    VUM.write colors i c
                    let Just vs = MP.lookup i graph
                    bs <- V.sequence $ V.map (\v -> dfs graph colors v (negate c)) vs
                    return $ V.and bs
                else
                    return (ci == c)
