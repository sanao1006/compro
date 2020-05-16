import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set as ST
import Data.Char
import Control.Monad
import Control.Applicative

-- 辺の情報 => V.Vector (Int (Int, Int)) ... (始点, (次の頂点, 距離))
-- グラフ => V.Vector (Int, Int) ... インデックス: 頂点, 要素:(次の頂点, 距離)
type Graph = V.Vector [(Int,Int)]
type Distance = VUM.IOVector Int
type Queue = ST.Set (Int, Int)
data Direction = Directed | Undirected
 
buildGraph :: Direction -> Int -> V.Vector (Int, (Int, Int)) -> Graph
buildGraph direction vertex pathInfo =
  case direction of 
    Directed   -> V.accumulate f (V.replicate vertex []) pathInfo
    Undirected -> V.accumulate f (V.replicate vertex []) pathInfo'
  where
    f = flip (:)
    g (a,(b,c)) = (b,(a,c))
    pathInfo' = pathInfo V.++ V.map g pathInfo
 
dijkstra :: Int -> Int -> Graph -> IO Distance
dijkstra vertex from graph = do
  distance <- VUM.replicate vertex (10^9) :: IO Distance
  VUM.write distance from 0 
  go graph distance (ST.singleton (0, from))

go :: Graph -> Distance -> Queue -> IO Distance
go graph distance queue
  | ST.null queue = return distance
  | otherwise = do
      tmp <- VUM.read distance from
      case compare acc tmp of
        LT -> updateQueue from acc vs distance queue' >>= go graph distance
        EQ -> updateQueue from acc vs distance queue' >>= go graph distance
        GT -> go graph distance queue'
  where
    ((acc, from), queue') = ST.deleteFindMin queue
    vs = graph V.! from
 
updateQueue :: Int -> Int -> [(Int, Int)] -> Distance -> Queue -> IO Queue
updateQueue _ _ [] _ queue = return queue
updateQueue from acc ((to, cost):xs) distance queue = do
    tmp <- VUM.read distance to
    case compare acc' tmp of
        LT -> VUM.write distance to acc' >> updateQueue from acc xs distance queue'
        _  -> updateQueue from acc xs distance queue
      where
        acc' = acc + cost
        queue' = ST.insert (acc', to) queue
