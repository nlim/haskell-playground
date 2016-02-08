module Euler31 where

-- Use a dynamic programming solution to find the number of ways
-- to assemble an amount from smaller amounts
-- Threading the DP table through the computation as a State monad

import Control.Monad.State
import qualified Data.Map as M (empty, Map, insert, lookup)

runWays :: Int -> Int
runWays n = evalState (ways denoms n) M.empty

denoms :: [Int]
denoms = [1, 2, 5, 10, 20, 50, 100, 200]

ways :: [Int] -> Int -> (State (M.Map (Int, [Int]) Int) Int)
ways _  0 = return 1
ways [] _ = return 0
ways (d:ds) n = do
  r1 <- lookupOrCompute n ds
  r2 <- lookupOrCompute (n-d) (d:ds)
  let v = r1 + r2
  modify $ (M.insert (n, d:ds) v)
  return v

lookupOrCompute :: Int -> [Int] -> (State (M.Map (Int, [Int]) Int) Int)
lookupOrCompute n ds = get >>= maybe (ways ds n) return . M.lookup (n,ds)
