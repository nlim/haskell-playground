module Block (numWays) where

import qualified Data.IntMap as M
import Control.Parallel.Strategies (Strategy, parBuffer,rdeepseq, withStrategy)
import Data.Set (Set)
import Data.List (foldl')
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
                           go (RowCombo i 0) results = [r ++ replicate i ThreeBlock | r <- results]
                           go (RowCombo 0 j) results = [r ++ replicate j FourFiveBlock | r <- results]
                           go (RowCombo i j) results =
                             go (RowCombo (i-1) j) [r ++ [ThreeBlock] | r <- results] ++ go (RowCombo i (j-1)) [r ++ [FourFiveBlock] | r <- results]

--- Get all the combinations of
--- choosing from the two types of blocks that add up to w
getCombos :: Int -> [RowCombo]
getCombos w = [ RowCombo i j | i <- [0..max3], j <- [0..max45], ((i*6) + (j*9)) == (2*w) ]
              where max3  = (2 * w) `div` 6
                    max45 = (2 * w) `div` 9


--- Get a List of all the x coordinates (times 2) where there is an intersection
--- of two blocks, that is not at either end
dJunctures :: [Block] -> [Int]
dJunctures bs = filter (\i -> i /= 0 && i /= w) (uncurry (:) r)
                where r :: (Int, [Int])
                      r = foldl' (\(t, ts) b -> (t + doubleBlockWidth b, t:ts)) (0,[]) bs
                      w :: Int
                      w = sum (map doubleBlockWidth bs)

--- We can build a Map from blockId => Set of blockIds that are valid to be adjacent to blockId
--- using a parallel Strategy
getValidAdjMap :: M.IntMap [Block] -> M.IntMap (Set Int)
getValidAdjMap blockMap = M.fromList (withStrategy (parBuffer 100 rdeepseq) (map (\i -> (i, valids i)) (M.keys bIdToDj)))
                       where
                          valids :: Int -> Set Int
                          valids bid = foldl' remBlockIdsForDj (Set.fromList (M.keys bIdToDj)) (bIdToDj M.! bid)
                          remBlockIdsForDj :: Set Int -> Int -> Set Int
                          remBlockIdsForDj s dj = foldl' (flip Set.delete) s (dJunctureToBlockIds M.! dj)
                          dJunctureToBlockIds = groupKeysByValue bIdToDj
                          bIdToDj = bIdToDj' blockMap

--- Inverts the IntMap so that we can efficiently find all the keys that map
--- to a given value element
groupKeysByValue :: M.IntMap [Int] -> M.IntMap [Int]
groupKeysByValue x = foldl' addKeyToAllValues M.empty $ M.keys x
  where addKeyToAllValues m' i' =  foldl' (\m'' j -> case M.lookup j m'' of
                                                       Just is -> M.insert j (i':is) m''
                                                       Nothing -> M.insert j [i'] m'')
                                           m'
                                           (x M.! i')


--- Given a IntMap which just the Block configurations labeled by an integer Key
--- Produce an IntMap which provides all the Block juncture points for a given integer key label for
--- the block configuration
bIdToDj' :: M.IntMap [Block] -> M.IntMap [Int]
bIdToDj' blockMap = M.fromList $ map (\i -> (i, dJunctures (blockMap M.! i))) (M.keys blockMap)



allPermutations :: Int -> [[Block]]
allPermutations width = do c <- getCombos width
                           getPermutations c

getLabelMap :: [a] -> M.IntMap a
getLabelMap as = M.fromList $ zip [1..] as

numWays :: Int -> Int -> Int
numWays width height = (sum . M.elems) (wayMap 1 (M.fromList [ (i,1) | i <- labels ]))
                       where
                         wayMap :: Int -> M.IntMap Int -> M.IntMap Int
                         wayMap n m
                               | n <= 0 = M.fromList [ (i,0) | i <- labels]
                               | n >= height = m
                               | otherwise = wayMap (n+1) $ M.fromList [(i, waysAdj i) | i <- labels]
                                 where
                                   waysAdj i = Set.foldl (\s j -> s + (m M.! j)) 0 (validAdjMap M.! i)
                         validAdjMap = getValidAdjMap labelMap
                         labels = M.keys labelMap
                         labelMap = getLabelMap (allPermutations width)

