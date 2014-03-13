import Util(foldLines)
import CharPairs


main :: IO ()
main = do
  fd <- foldLines accumFreqFromLine emptyFreqData
  putStrLn $ show $ calcFreqStats fd
