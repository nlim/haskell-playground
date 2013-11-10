{-# LANGUAGE BangPatterns #-}
import Control.Concurrent
import Control.Monad
import System.IO
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy as L
import System.Environment
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad (forever, forM_, replicateM_)

nrWorkers = 2

main :: IO ()
main = channelProg

channelProg = do files <- getArgs
                 str <- newChan
                 fileChan <- newChan
                 forM_ [1..nrWorkers] (\_ -> forkIO $ worker str fileChan)
                 forM_ files (writeChan fileChan)
                 printNrResults (length files) str

printNrResults i var = replicateM_ i (readChan var >>= putStrLn)

worker :: Chan String -> Chan String -> IO ()
worker str fileChan = forever (readChan fileChan >>= hashAndPrint str)

hashAndPrint str f = do
        bs <- L.readFile f
        let !h = show $ md5 bs
        writeChan str (f ++ ": " ++ h)


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
