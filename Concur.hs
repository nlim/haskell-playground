import Control.Concurrent
import Control.Monad
import System.IO
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

main :: IO ()

main = funTest

funTest :: IO ()
funTest = do
  tv <- newTVarIO 0
  forkIO $ (readTVarIO tv >>= (putStrLn . show))
  forkIO $ replicateM_ 10 $ atomicAddV tv 1
  forkIO $ replicateM_ 10 $ atomicAddV tv 2
  putStrLn "Done Forking"


atomicAddV :: TVar Int -> Int -> IO ()
atomicAddV tv x = atomically (addV x tv)

addV :: Int -> TVar Int -> STM ()
addV x = (flip modifyTVar) $ (+x)


prepend :: a -> TVar [a] -> STM ()
prepend x = (flip modifyTVar) ((:) x)

transPrependIO :: TVar[a] -> a -> IO ()
transPrependIO tv x = atomically (prepend x tv)

repeatPutChar :: Char -> Int -> IO ()
repeatPutChar c i = replicateM_ i (putChar c)

interleaveChars :: IO ()
interleaveChars = do
  hSetBuffering stdout NoBuffering
  forkIO (repeatPutChar 'A' 10000)
  (repeatPutChar 'B' 10000)
