import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (foldl', sortBy)
import System.IO

main :: IO ()
main = readLoop emptyFreqData


readLoop :: FreqData -> IO ()
readLoop fd = do iseof <- isEOF
                 if iseof
                 then (putStrLn . freqMessage) fd
                 else do inputStr <- getLine
                         readLoop (accumFreqFromLine fd inputStr)




type CharPair = (Char, Char)

type InverseHisto a = Map.Map Int (Set.Set a)

type Histo a = (Map.Map a Int)

data FreqData = FreqData {
  inverseHisto :: InverseHisto CharPair,
  histo :: Histo CharPair,
  size :: Int
} deriving (Show)

limit = 100

testPairs :: [CharPair]
testPairs = replicate 10 ('f', 'b')

freqMessage :: FreqData -> String
freqMessage = show


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

emptyFreqData :: FreqData
emptyFreqData = FreqData Map.empty Map.empty 0







