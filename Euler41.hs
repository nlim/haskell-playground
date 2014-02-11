module Euler41 where

isPrime :: Int -> Bool
isPrime n
  | n < 0 = False
  | n == 0 = False
  | n == 1 = False
  | otherwise = ((==1) . length . factors) n

factors :: Int -> [Int]
factors n = filter ((== 0) . (mod n)) [1..r]
  where r = (ceiling . sqrt . fromIntegral) n

undigits :: [Int] -> Int
undigits = (undigits' 1 0) . reverse
  where undigits' _ accum [] = accum
        undigits' place accum (d:ds) = undigits' (10 * place) (accum + (place*d)) ds

pandigitals :: Int -> [Int]
pandigitals i = ((map undigits) . perms) $ reverse [1..i]

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = (map (x:) next) ++ (concat $ map (amongst x) next)
  where next = perms xs

amongst :: a -> [a] -> [[a]]
amongst v as = amongst' [] as
  where amongst' ls [] = []
        amongst' ls (r:rs) = y' : (amongst' l' rs)
          where l' = ls ++ [r]
                y' = l' ++ (v:rs)




main = putStrLn $ show $ take 1 $ filter isPrime  $ pandigitals 9
