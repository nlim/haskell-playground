import qualified Data.IntMap as M
import System.Environment
import Control.Parallel.Strategies (Strategy, runEval, parMap, rpar, evalTuple2, rseq)
import Data.Set (Set)
import qualified Data.Set as Set


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


validAdj :: Int -> Int -> (M.IntMap [Block]) -> Bool
validAdj x y labelMap = validAdjacent (labelMap M.! x) (labelMap M.! y)

dJunctures :: [Block] -> [Int]
dJunctures bs = filter (\i -> i /= 0 && i /= w) ((fst r):(snd r))
                where r = (foldl (\(t, ts) b -> (t + (doubleBlockWidth b), t:ts)) (0,[]) bs)
                      w = sum (map doubleBlockWidth bs)

getValidAdjMap :: M.IntMap [Block] -> M.IntMap (Set Int)
getValidAdjMap blockMap = M.fromList $ zip (M.keys bIdToDj) (parMap rseq valids (M.keys bIdToDj))
                       where
                          valids :: Int -> (Set Int)
                          valids bid = foldl
                            (\s dj -> remBlockIdsForDj dj s)
                            (Set.fromList (M.keys bIdToDj))
                            (bIdToDj M.! bid)

                          remBlockIdsForDj :: Int -> (Set Int) -> (Set Int)
                          remBlockIdsForDj dj s = foldl (\s' bid ->  Set.delete bid s') s (dJunctureToBlockIds M.! dj)

                          dJunctureToBlockIds = dJunctureToBlockIds' bIdToDj

                          bIdToDj = bIdToDj' blockMap

dJunctureToBlockIds' :: M.IntMap [Int] -> M.IntMap [Int]
dJunctureToBlockIds' bIdToDj = foldl
  (\m i -> foldl
    (\m' j -> case (M.lookup j m') of
                Just is -> M.insert j (i:is) m'
                Nothing -> M.insert j [i] m')
    m
    (bIdToDj M.! i))
  M.empty
  (M.keys bIdToDj)



bIdToDj' :: (M.IntMap [Block]) -> M.IntMap [Int]
bIdToDj' blockMap = M.fromList $ map (\i -> (i, dJunctures (blockMap M.! i))) (M.keys blockMap)



allPermutations :: Int -> [[Block]]
allPermutations width = do c <- getCombos(width)
                           p <- getPermutations(c)
                           return p

getLabelMap :: [a] -> (M.IntMap a)
getLabelMap as = M.fromList $ zip [1..] as

numWays :: Int -> Int -> Int
numWays width height = (sum . M.elems) wayMap
                       where
                         wayMap = wayMapH 1 (M.fromList [ (i,1) | i <- labels ])
                         wayMapH :: Int -> (M.IntMap Int) -> (M.IntMap Int)
                         wayMapH n m
                                | n >= height = m
                                --- We can parallelize this step
                                | otherwise = wayMapH (n+1) $ M.fromList [(i, waysAdj i) | i <- labels]
                                  where
                                    waysAdj i = Set.foldl (\s j -> s + (m M.! j)) 0 (validAdjMap M.! i)


                         validAdjMap = getValidAdjMap labelMap
                         labels = M.keys labelMap
                         labelMap = getLabelMap (allPermutations width)

parseInt :: [Char] -> Int
parseInt s = read s

-- Run this
main :: IO ()
main = do
  [ws,hs] <- getArgs
  putStrLn $ show $ numWays (parseInt ws) (parseInt hs)

--- time ./Block 48 10  +RTS -N4 -l
--- 806844323190414
---
--- real  0m2.889s
--- user  0m10.923s
--- sys 0m0.445s

