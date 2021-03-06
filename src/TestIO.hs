import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
  inh <- openFile "./test_files/input.txt" ReadMode
  outh <- openFile "./test_files/output.txt" WriteMode
  mainloop inh outh
  hClose inh
  hClose outh

forever :: IO a -> IO b
forever p = p >> forever p

echo :: IO ()
echo = getLine >>= putStrLn

cat :: IO ()
cat = forever echo



mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = do ineof <- hIsEOF inh
                       if ineof
                         then return ()
                         else do inpStr <- hGetLine inh
                                 hPutStrLn outh (map toUpper inpStr)
                                 mainloop inh outh
