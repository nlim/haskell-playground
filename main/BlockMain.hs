import Block
import System.Environment

parseInt :: String -> Int
parseInt = read

main :: IO ()
main = do
  [ws,hs] <- fmap (map parseInt) getArgs
  print $ numWays ws hs

