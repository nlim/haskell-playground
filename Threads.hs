module Main where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Data.Time
import System.Environment

main = do
  mv <- newEmptyMVar
  start <- getCurrentTime
  loop mv =<< read . head <$> getArgs
  end <- getCurrentTime
  putStrLn $ "creation time: " ++ show (diffUTCTime end start)
  putMVar mv 0
  v <- takeMVar mv
  fin <- getCurrentTime
  putStrLn $ "Var Value: " ++ (show v) ++ " Message time: " ++ show (diffUTCTime fin end)

loop :: MVar Int -> Int -> IO ()
loop mv n | n <= 0 = return ()
          | otherwise = do forkIO $ do
                              m <- takeMVar mv
                              putMVar mv $! m+1
                           loop mv (n-1)
