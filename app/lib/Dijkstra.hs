import qualified Data.ByteString.Char8 as BC
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set as ST
import Data.Char
import Control.Monad
import Control.Applicative
 
type Graph = V.Vector [(Int,Int)]
type Distance = VUM.IOVector Int
type Queue = ST.Set (Int, Int)
data Direction = Directed | Undirected
 
buildGraph :: Direction -> Int -> V.Vector (Int, (Int, Int)) -> Graph
buildGraph direction vertex pathInfo =
  case direction of 
    Directed -> V.accumulate step (V.replicate vertex []) pathInfo
    Undirected -> V.accumulate step (V.replicate vertex []) pathInfo'
  where
    step xs v = v:xs
    pathInfo' = pathInfo V.++ (V.map (\(a,(b,c)) -> (b,(a,c))) pathInfo)
 
dijkstra :: Int -> Int -> Graph -> IO Distance
dijkstra vertex from graph = do
  distance <- VUM.replicate vertex (10^9) :: IO Distance
  VUM.write distance from 0 
  trav graph distance (ST.singleton (0, from))
  where
    trav :: Graph -> Distance -> Queue -> IO Distance
    trav graph distance queue
      | ST.null queue = return distance
      | otherwise = do
          tmp <- VUM.read distance from
          case compare acc tmp of
            LT -> update from acc distance queue' >>= trav graph distance
            EQ -> update from acc distance queue' >>= trav graph distance
            GT -> trav graph distance queue'
      where
        ((acc, from), queue') = ST.deleteFindMin queue
 
    update :: Int -> Int -> Distance -> Queue -> IO Queue
    update f a dist q = update' a dist (graph V.! f) q

    update' :: Int -> Distance -> [(Int, Int)] -> Queue -> IO Queue
    update' _ _ [] q = return q
    update' a dist ((to, cost):xs) q = do
      tmp <- VUM.read dist to
      case compare a' tmp of
        LT -> VUM.write dist to a' >> update' a dist xs q'
        _ -> update' a dist xs q
      where
        a' = a + cost
        q' = ST.insert (a', to) q

convToPathInfo bs = (s-1, (t-1, d))
  where 
      Just (s, bs') = parseInt bs
      Just (t, bs'') = parseInt bs'
      Just (d, _) = parseInt bs'' 
      parseInt = BC.readInt . BC.dropWhile isSpace

main = do
  -- n を頂点数、m を辺の数とする
  -- 辺の情報 => V.Vector (Int (Int, Int)) ... (始点, (次の頂点, 距離))
  -- グラフ => V.Vector (Int, Int) ... インデックス: 頂点, 要素:(次の頂点, 距離)
  [n,m] <- map read . words <$> getLine :: IO [Int]
  pathInfo <- V.replicateM m $ BC.getLine >>= return . convToPathInfo
  let graph = buildGraph Directed n pathInfo
  --let graph = buildGraph Undirected n pathInfo
  dist <- dijkstra n 0 graph
  forM_ [0..n-1] $ \i -> do
    VUM.read dist i >>= print
