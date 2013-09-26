import Control.Concurrent
import Control.Monad
import System.IO

main :: IO ()

main = interleaveChars

repeatPutChar :: Char -> Int -> IO ()
repeatPutChar c i = replicateM_ i (putChar c)

interleaveChars :: IO ()
interleaveChars = do
  hSetBuffering stdout NoBuffering
  forkIO (repeatPutChar 'A' 10000)
  (repeatPutChar 'B' 10000)
