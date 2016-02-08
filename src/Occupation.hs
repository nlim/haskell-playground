module Occupation where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List

{--
-
- From 101 Puzzles in Thought and Logic, problem 6.
-
- Clark, Jones, Morgan, and Smith are four men whose occupation are
- butcher,
- druggist, grocer, and policeman, though not necessarily respectively.
-
- * Clark and Jones are neighbors and take turns driving each other to work
- * Jones makes more money than Morgan
- * Clark beats Smith regularly at bowling
- * The butcher always walks to work
- * The policeman does not live near the druggist
- * The only time the grocer and the policeman ever met was when the
- policeman
-   arrested the grocer for speeding
-   * The policeman makes more money than the druggist or the grocer
-
-   WHAT IS EACH MAN'S OCCUPATION?
-
-   --}

data Man = Clark | Jones | Morgan | Smith
            deriving (Eq, Ord, Enum, Show, Read)

data Job = Butcher | Druggist | Grocer | Policeman
            deriving (Eq, Ord, Enum, Show, Read)

men :: [Man]
men  = [Clark, Jones, Morgan, Smith]

jobs :: [Job]
jobs = [Butcher, Druggist, Grocer, Policeman]

pairs :: [[(Man, Job)]]
pairs = map (zip men) $ List.permutations jobs

pairsSet :: Set (Set (Man, Job))
pairsSet = Set.fromList $ map Set.fromList pairs

invalid :: [(Man, Job)] -> Set (Man, Job) -> Bool
invalid ps s = not $ any ((flip Set.member) s) ps

occupation :: Set (Set (Man, Job))
occupation = Set.filter (invalid [(Clark, Butcher), (Jones, Butcher)]) pairsSet


dcfM :: Double -> Int -> Double -> Double -> Double
dcfM interestRate growthYears growth termalGrowth = growthValue + terminalValue where
  growthValue  :: Double
  growthValue = sum $ take growthYears $ List.iterate (* (discount * growth)) 1.0
  terminalValue = sum $ take 1000 $ List.iterate (* (discount * termalGrowth)) terminalEarning
  terminalEarning :: Double
  terminalEarning = (discount * growth) ** (fromIntegral growthYears)
  discount :: Double
  discount = 1.0 - interestRate


dcfM2 :: Double -> Double -> Double -> Double
dcfM2 _ _ n | n <= 0 = 0.0
dcfM2 d g n = ((g ** n) * ((1.0 - d) ** n)) + (dcfM2 d g (n-1))


newtype StartingAmount = StartingAmount { unStarting :: Double }

newtype TargetAmount = TargetAmount { unTarget :: Double }

newtype GrowthRate = GrowthRate{ unGrowth:: Double }

newtype CashFlow = CashFlow { unCashFlow :: Double }

netWorthsAfterYears :: StartingAmount -> GrowthRate -> CashFlow -> Int -> [Double]
netWorthsAfterYears (StartingAmount sa) (GrowthRate gr) (CashFlow cf) y =
    List.scanl (\a i -> a * gr + cf) sa [1..y]


netWorths :: StartingAmount -> TargetAmount -> GrowthRate -> CashFlow -> [Int]
netWorths (StartingAmount c) (TargetAmount t) (GrowthRate r) (CashFlow s) =
  if (nextValue < c)
    then [round nextValue]
    else (round nextValue) : netWorths (StartingAmount c) (TargetAmount nextValue) (GrowthRate r) (CashFlow s) where
      nextValue = (t / r) - s

