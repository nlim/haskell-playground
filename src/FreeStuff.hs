module FreeStuff where


import Control.Monad.Free
import Control.Monad.State
import System.Exit (exitSuccess)


data TeletypeF x = PutStrLn String x
                 | GetLine (String -> x)
                 | ExitSuccess

instance Functor TeletypeF where
  fmap f (PutStrLn str x) = PutStrLn str (f x)
  fmap f (GetLine      k) = GetLine (f . k)
  fmap _ (ExitSuccess)    = ExitSuccess

type Teletype = Free TeletypeF

putStrLn' :: String -> Teletype ()
putStrLn' str = liftF $ PutStrLn str ()

getLine' :: Teletype String
getLine' = liftF $ GetLine id

-- runF :: TeletypeF r -> IO r
-- runF (PutStrLn str t) = putStrLn str

run :: Teletype r -> IO r
run (Pure r) = return r
run (Free (PutStrLn str t)) = putStrLn str >>  run t
run (Free (GetLine  f    )) = getLine      >>= run . f
run (Free (ExitSuccess))    = exitSuccess

data Buffers i o = Buffers { input :: [i], output :: [o] } deriving (Show)

putOut :: o -> (Buffers i o) -> (Buffers i o)
putOut v (Buffers i o) = Buffers i (v:o)

printB :: String -> StringConsole ()
printB s = modify $ putOut s

getB :: StringConsole String
getB = do buffers <- get
          let (m, buffers') = pullIn buffers
          put buffers'
          return m


pullIn :: (Buffers i o) -> (i,(Buffers i o))
pullIn (Buffers is o) = (head is, Buffers (tail is) o)


type StringConsole a = State (Buffers String String) a

runB :: Teletype r -> StringConsole r
runB (Pure r) = return r
runB (Free (PutStrLn str t)) = printB str >> runB t
runB (Free (GetLine f)) = getB >>= runB . f
runB (Free ExitSuccess) = error "ExitSuccess"


echoB :: ((), Buffers String String)
echoB = runState (runB echo) (Buffers ["Do you hear me?"] [])

echo :: Teletype ()
echo = do
  str <- getLine'
  putStrLn' str
  putStrLn' "Finished"

intro :: Teletype ()
intro = do
  putStrLn' "What is your name"
  name <- getLine'
  putStrLn' $ "Hello " ++ name ++ " my name is Teletype"
  let x = length name
  putStrLn' $ "Wow you name is " ++ (show x) ++ " letters long"


program :: IO ()
program = run echo
