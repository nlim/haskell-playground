
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


data Row

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

dJunctures :: [Block] -> [Int]
dJunctures bs = (fst r):(snd r)
                where r = (foldl (\(t, ts) b -> (t + (doubleBlockWidth b), t:ts)) (0,[]) bs)



allPermutations :: Int -> [[Block]]
allPermutations width = do c <- getCombos(width)
                           p <- getPermutations(c)
                           return p


--- Run this
main :: IO ()
main = do
  putStrLn (show (getCombos 48))
  putStrLn (show (length (allPermutations 48)))



