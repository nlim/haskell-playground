import qualified Data.Map as Map
import System.Environment
--- Solves the following problem using a dynamic programming solution
--- using Haskell parallelism techniques and benchmarking
--- learned in Parallel and Concurrent Haskell

--- Problem: There are two types of blocks, with sizes (w x h): 4.5 x 1 and 3 x 1.
--- we want to know how many ways can you stack up these blocks such that:
---   * Make a rectangular with width: W and height H
---   * Adjacent Rows can't have two blocks sitting next to each other at the same x coordinate

data Block = ThreeBlock | FourFiveBlock deriving (Eq, Ord, Show)

doubleBlockWidth :: Block -> Int
doubleBlockWidth ThreeBlock = 6
doubleBlockWidth FourFiveBlock = 9


data RowCombo = RowCombo { numThrees :: Int, numFourFives :: Int } deriving (Eq, Ord, Show)



getPermutations :: RowCombo -> [[Block]]
getPermutations rc = go rc [[]]
                     where go :: RowCombo -> [[Block]] -> [[Block]]
                           go (RowCombo 0 0) results = results
                           go (RowCombo i 0) results = [r ++ (take i (repeat ThreeBlock)) | r <- results]
                           go (RowCombo 0 j) results = [r ++ (take j (repeat FourFiveBlock)) | r <- results]
                           go (RowCombo i j) results = (go (RowCombo (i-1) j) ([r ++ [ThreeBlock] | r <- results])) ++ (go (RowCombo i (j-1)) ([r ++ [FourFiveBlock] | r <- results]))


getWidth :: RowCombo -> Float
getWidth (RowCombo i j) = (3 * x) + (4.5 * y)
                        where x = fromIntegral i :: Float
                              y = fromIntegral j :: Float

getCombos :: Int -> [RowCombo]
getCombos w = [ RowCombo i j | i <- [0..max3], j <- [0..max45], ((i*6) + (j*9)) == (2*w) ]
              where max3  = ((2*w) `div` 6)
                    max45 = ((2*w) `div` 9)

validAdjacent :: [Block] -> [Block] -> Bool
validAdjacent perm1 perm2 = (w2 == w1) && (not (any (\p -> (p /= 0) && (p /= w2) && p `elem` (dJunctures perm2)) (dJunctures perm1)))
                            where w2 = sum (map doubleBlockWidth perm2)
                                  w1 = sum (map doubleBlockWidth perm1)


validAdj :: Int -> Int -> (Map.Map Int [Block]) -> Bool
validAdj x y labelMap = maybe False id $ do bx <- Map.lookup x labelMap
                                            by <- Map.lookup y labelMap
                                            return (validAdjacent bx by)

dJunctures :: [Block] -> [Int]
dJunctures bs = (fst r):(snd r)
                where r = (foldl (\(t, ts) b -> (t + (doubleBlockWidth b), t:ts)) (0,[]) bs)



allPermutations :: Int -> [[Block]]
allPermutations width = do c <- getCombos(width)
                           p <- getPermutations(c)
                           return p

getLabelMap :: [a] -> (Map.Map Int a)
getLabelMap as = Map.fromList $ zip [1..] as

numWays :: Int -> Int -> Int
numWays width height = sum (Map.elems wayMap)
                       where
                         wayMap = wayMapH 1 (Map.fromList [ (i,1) | i <- labels ])
                         wayMapH :: Int -> (Map.Map Int Int) -> (Map.Map Int Int)
                         wayMapH n m
                                | n >= height = m
                                | otherwise = wayMapH (n+1) (Map.fromList [(i, waysAdj i) | i <- labels ])
                                  where
                                    waysAdj i = foldl (\s j -> if (validAdj i j labelMap) then s + (maybe 0 id (Map.lookup j m)) else s) 0 labels
                         labels = Map.keys labelMap
                         labelMap = getLabelMap (allPermutations width)

parseInt :: [Char] -> Int
parseInt s = read s

-- Run this
main :: IO ()
main = do
  [ws,hs] <- getArgs
  putStrLn $ show $ numWays (parseInt ws) (parseInt hs)



