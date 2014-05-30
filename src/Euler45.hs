module Euler45 where

import qualified Data.Map as M (empty, Map, insert, lookup, foldlWithKey')
import qualified Data.List as L (foldl')

triangleNumber n = div (n * (n + 1)) 2

pentagonalNumber n = div (n * (3 * n - 1)) 2

hexagonalNumber n = n * (2 * n - 1)

numbersWithIndicies limit = concatMap mkSeq [triangleNumber, pentagonalNumber, hexagonalNumber] where
  mkSeq f = takeWhile ((< limit) . snd) [ (n, f n) | n <- [1..]]

data Histo = Histo { numberToIndices :: M.Map Int [Int] } deriving Show

emptyHisto = Histo M.empty

insertH (Histo ni) (i, v) = Histo ni' where
  ni' = case (M.lookup v ni) of
          Just is -> M.insert v (i:is) ni
          Nothing -> M.insert v [i] ni

lookupHisto :: Int -> Histo
lookupHisto limit = L.foldl' insertH emptyHisto $ numbersWithIndicies limit

numbersWithOccurrences :: Int -> Int -> [(Int, [Int])]
numbersWithOccurrences limit occurrences = M.foldlWithKey' addNumber emptyNums m where
  addNumber :: [(Int, [Int])] -> Int -> [Int] -> [(Int, [Int])]
  addNumber ns n is = if (length is == occurrences) then (:) (n, is) ns else ns
  m = numberToIndices $ lookupHisto limit
  emptyNums = []

