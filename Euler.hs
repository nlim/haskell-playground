import Control.Monad.State.Lazy
import qualified Data.Map as M

--- Solving some problems at http://projecteuler.net/
fibs :: Int -> [Int]
fibs n = fibsH n []
         where fibsH 0 l = l
               fibsH x [] = fibsH (x-1) (1:[])
               fibsH x (h:[]) = fibsH (x-1) (h:h:[])
               fibsH x (h:h2:t) = fibsH (x-1) ((h+h2):h:h2:t)

--- Prints out the sum of all multiples of f or s under limit
sumMultiplesUnder :: Int -> Int -> Int -> Int
sumMultiplesUnder limit f s = go f s 0
    where go :: Int -> Int -> Int -> Int
          go fi si sum
            | (fi < limit) && (si < limit) = go (fi+f) (si+s) (sum + fi + si)
            | (fi < limit) = go (fi+f) (si) (sum + fi)
            | (si < limit) = go (fi) (si+s) (sum + si)
            | otherwise = sum


--- Prints out the sum of of all the even fibonacci numbers
--- you can pass in 4000000 and this doesn't run out of stack
evenFibsSum :: Int -> Int
evenFibsSum limit = go limit 1 1 0
                    where
                    go :: Int -> Int -> Int -> Int -> Int
                    go l f s sum
                      | (f > l) = sum
                      | ((mod f 2) == 0) = go l (f + s) f (sum+f)
                      | otherwise        = go l (f + s) f sum




main :: IO ()
main = other

{- Project Euler 92 -}

validEnd :: Int -> Bool
validEnd i = (i == 1 || i == 89)

numTerminatingOn :: Int -> [Int] -> Int
numTerminatingOn t is
  | validEnd t = length $ filter (== t) (evalState (mapM terminatorS is) M.empty) 
  | otherwise = 0


terminatorS :: Int -> State (M.Map Int Int) Int
terminatorS i = do m <- get 
                   case (M.lookup i m) of
                     Just j -> return j
                     Nothing -> do t <- terminator (step i)
                                   modify (M.insert i t)
                                   return t


terminator :: Int -> State (M.Map Int Int) Int
terminator i
  | validEnd i = return i
  | otherwise = terminatorS i

step :: Int -> Int
step = sum . (map (\x -> x*x)) . digits

digits :: Int => [Int]
digits i = go [] i
  where
    go [] 0 = [0]
    go ds 0 = ds
    go ds j = go ((mod j 10):ds) (div j 10)


other :: IO ()
other = do
  putStrLn (show (sumMultiplesUnder 1000 3 5))
  putStrLn (show (fibs 10))
  putStrLn (show (evenFibsSum 4000000))
