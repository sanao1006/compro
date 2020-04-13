import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

-- Union-Find
type Parent = VUM.IOVector Int
type Size = VUM.IOVector Int

initUF :: Int -> IO (Parent, Size)
initUF n = do
    par <- VU.unsafeThaw $! VU.generate n (id :: Int -> Int)
    siz <- VUM.replicate n (1 :: Int)
    return (par, siz)
 
root :: Int -> Parent -> IO Int
root x par = do
    r <- VUM.read par x
    case compare x r of
        EQ -> return x
        _ -> do
            r' <- root r par
            VUM.write par x r'
            return r'
 
same :: Int -> Int -> Parent -> IO Bool
same x y par = do
    x' <- root x par
    y' <- root y par
    return $! x' == y'
 
unite :: Int -> Int -> Parent -> Size -> IO Bool
unite x y par siz = do
    x' <- root x par
    y' <- root y par
    sx <- VUM.read siz x'
    sy <- VUM.read siz y'
    case compare x' y' of 
        EQ -> return False
        _ -> do
            case compare sx sy of
                LT -> do
                    VUM.modify siz (+sx) y'
                    VUM.write par x' y'
                _ -> do
                    VUM.modify siz (+sy) x'
                    VUM.write par y' x'
            return True
 
size :: Int -> Parent -> Size -> IO Int
size x par siz = root x par >>= VUM.read siz