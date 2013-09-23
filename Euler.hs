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
main = do
  putStrLn (show (sumMultiplesUnder 1000 3 5))
  putStrLn (show (fibs 10))
  putStrLn (show (evenFibsSum 4000000))
