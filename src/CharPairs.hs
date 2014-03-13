module CharPairs(FreqData, emptyFreqData, calcFreqStats, accumFreqFromLine, pairsFromLine) where

import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl', sortBy)
import System.IO

type CharPair = (Char, Char)

type InverseHisto a = Map.Map Int (Set.Set a)

type Histo a = (Map.Map a Int)

data FreqData = FreqData {
  inverseHisto :: InverseHisto CharPair,
  histo :: Histo CharPair,
  size :: Int
} deriving (Show)

emptyFreqData :: FreqData
emptyFreqData = FreqData Map.empty Map.empty 0

calcFreqStats :: FreqData -> ([(CharPair, Float)], Float)
calcFreqStats fd = (topPairs, percentage numTopPairs)
  where
    limit :: Int
    limit = 10
    s :: Int
    s = size fd
    percentage i = (100.0 * ((fromIntegral i) / (fromIntegral s)))
    topCounts :: [Int]
    topCounts = ((map fst) . Map.toDescList . inverseHisto) fd
    allTopPairs :: [(CharPair, Float)]
    allTopPairs = [ (cp, percentage i) | i <- topCounts, cp <- (Set.toList ((inverseHisto fd) Map.! i))]
    topPairs = take limit allTopPairs
    numTopPairs = foldl' (\sum cp -> sum + ((histo fd) Map.! cp)) 0 ((map fst) topPairs)


accumFreqFromLine :: FreqData -> String -> FreqData
accumFreqFromLine fd s = foldl' loadCharPair fd (pairsFromLine s)

pairsFromLine :: String -> [CharPair]
pairsFromLine s = (words s) >>= pairsFromWord

pairsFromWord :: String -> [CharPair]
pairsFromWord = pairsHelper []
  where
    pairsHelper ps (c1:c2:cs) = pairsHelper ((toLower c1, toLower c2):ps) (c2:cs)
    pairsHelper ps _ = ps

loadCharPair :: FreqData -> CharPair -> FreqData
loadCharPair (FreqData ih h s) cp = FreqData ih' h' (s+1)
  where
    h' = Map.insert cp c' h
    c' = c + 1
    c = maybe 0 id (Map.lookup cp h)
    ih' = (addCp . removeCp) ih
            where
              removeCp ih'' = maybe ih''
                                    (\s -> Map.insert c (Set.delete cp s) ih'')
                                    (Map.lookup c ih'')
              addCp ih''    = maybe (Map.insert c' (Set.fromList [cp]) ih'')
                                    (\s -> Map.insert c' (Set.insert cp s) ih'')
                                    (Map.lookup c' ih'')
