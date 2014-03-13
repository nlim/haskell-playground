import Euler(isPrime, factors, undigits)
import Data.List(find)

pandigitals :: Int -> [Int]
pandigitals i = ((map undigits) . perms) $ reverse [1..i]


--- Permutes the List by modifying the tail
perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = (map (x:) next) ++ (concat $ map (amongst x) next) where next = perms xs

amongst :: a -> [a] -> [[a]]
amongst v as = amongst' [] as
  where amongst' ls [] = []
        amongst' ls (r:rs) = y' : (amongst' l' rs)
          where l' = ls ++ [r]
                y' = l' ++ (v:rs)

-- 9-digit pandigitals cannot be prime as sum [1..9]=45 is divisible by 3
-- 8-digit pandigitals cannot be prime as sum [1..8]=36 is divisible by 3
main = do
  let eg = [3, 2, 1]
  putStrLn $ "Example Perms" ++ (show eg) ++ " = " ++ (show $ perms eg)
  putStrLn $ show $ find isPrime $ pandigitals 7
