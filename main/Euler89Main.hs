import Euler89
import Util

main :: IO ()
main = transformLines replaceLine

replaceLine :: String -> String
replaceLine s = case (fmap (minimalNumerals . sum . (map numeralValue)) $ parseNumerals s) of
                  Just ns -> concatMap show ns
                  Nothing -> s


charactersSaved :: IO ()
charactersSaved = do
  result <- foldLines accumDiffs 0
  putStrLn $ show result

accumDiffs :: Int -> String -> Int
accumDiffs total s = total + minimizingDiff s

minimizingDiff :: String -> Int
minimizingDiff s = case (fmap (minimalNumerals . sum . (map numeralValue)) $ parseNumerals s) of
                     Just ns -> (length s) - (length ns)
                     Nothing -> 10000000000 

