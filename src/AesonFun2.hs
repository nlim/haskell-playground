{-# LANGUAGE DeriveGeneric #-}

module AesonFun2 where

import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hPutStrLn, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import qualified Data.ByteString as BS

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL

data LogIn = LogIn { username :: String, password :: String } deriving (Show, Generic)

instance FromJSON LogIn

main2 :: IO ()
main2 = withSocketsDo $ do
  sock <- listenOn $ PortNumber 3333
  putStrLn $ "Listening on " ++ "3333"
  sockHandler sock


sockHandler :: Socket -> IO ()
sockHandler sock = do
  (handle, _, _) <- accept sock
  hSetBuffering handle NoBuffering
  _ <- forkIO $ commandProcessor handle
  sockHandler sock


commandProcessor :: Handle -> IO ()
commandProcessor handle = do
  line <- BS.hGetLine handle
  let lazyLine = BL.fromChunks [line]
  let Just login = decode lazyLine :: Maybe LogIn
  _ <- hPutStrLn handle $ show login
  _ <- hPutStrLn handle "."
  commandProcessor handle
