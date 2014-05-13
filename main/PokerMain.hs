import Poker
import System.Environment
import Control.Monad

--- Main
--
main :: IO ()
main = forever $ do
  putStrLn "Input Hand: e.g. C1 C2 H3 C4 D5"
  line <- getLine
  putStrLn (show (fmap getHandType (parseHand line)))


prog :: IO ()
prog = do
  line <- getLine
  putStrLn $ show $ Ace
