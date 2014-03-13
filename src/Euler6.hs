
square x = x * x

sumOfSquares :: [Int] -> Int
sumOfSquares = sum . (map square)

squareOfSum :: [Int] -> Int
squareOfSum = square . sum

diff :: [Int] -> Int
diff is = s1 - s2
  where s1 = squareOfSum is
        s2 = sumOfSquares is


main :: IO ()
main = putStrLn $ (show . diff) [1..10] 
