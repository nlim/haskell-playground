module Euler15 where
--
-- Use a dynamic programming solution to find the number of ways
-- to travel through a grid from top corner to bottom corner

import Control.Monad.State
import qualified Data.Map as M (empty, Map, insert, lookup)

runWays :: (Int, Int) -> Int
runWays t = evalState (ways t) $ M.empty

ways :: (Int, Int) -> State (M.Map (Int, Int) Int) Int
ways (0, _) = return 1
ways (_, 0) = return 1
ways (x, y) | (x < 0 || y < 0) = return 0
            | otherwise = do
                v1 <- lookupOrCompute (x-1,y)
                v2 <- lookupOrCompute (x, y-1)
                let v = v1 + v2
                modify $ M.insert (x,y) v
                return v

lookupOrCompute :: (Int, Int) -> State (M.Map (Int, Int) Int) Int
lookupOrCompute (x,y) = do
  m <- get
  case (M.lookup (x,y) m) of
    Just v  -> return v
    Nothing -> ways (x,y)

