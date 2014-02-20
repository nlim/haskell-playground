data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Show, Eq, Enum, Bounded)
data Month = Jan | Feb | Mar | Apr | May | Jun | July | Aug | Sept | Oct | Nov | Dec deriving (Show, Eq, Enum, Bounded)


class Eq s => Cycle s where
  start :: s
  next  :: s -> s
  back  :: s -> s
  end   :: s

instance Cycle Day where
  start  = minBound
  end    = maxBound
  next d = if d == end then start else succ d
  back d = if d == start then end else pred d

instance Cycle Month where
  start  = minBound
  end    = maxBound
  next d = if d == end then start else succ d
  back d = if d == start then end else pred d

cycleC :: Cycle a => a -> [a]
cycleC a = a : (cycleC $ next a)

newtype DayOfMonth = DayOfMonth { intValue :: Int } deriving (Show)

newtype Year = Year { unYear :: Int } deriving (Show, Ord, Eq)
nextYear :: Year -> Year
nextYear (Year i) = Year (i+1)
lastYear :: Year -> Year
lastYear (Year i) = Year (i-1)

data Date = Date { dayOfWeek :: Day, dayOfMonth :: DayOfMonth, year :: Year, month :: Month} deriving (Show)

--- 1 Jan 1900 was a Monday.
--- Thirty days has September, --- April, June and November.
--- All the rest have thirty-one,
--- Saving February alone,
--- Which has twenty-eight, rain or shine.
--- And on leap years, twenty-nine.
--- A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
--- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


daysInMonth :: Month -> Year -> Int
daysInMonth Feb  y = if isLeap y then 29 else 28
daysInMonth Jan  _ = 31
daysInMonth Mar  _ = 31
daysInMonth Apr  _ = 30
daysInMonth May  _ = 31
daysInMonth Jun  _ = 30
daysInMonth July _ = 31
daysInMonth Aug  _ = 31
daysInMonth Sept _ = 30
daysInMonth Oct  _ = 31
daysInMonth Nov  _ = 30
daysInMonth Dec  _ = 31

isLeap :: Year -> Bool
isLeap (Year i) = if multOf 400 then True
                  else and [multOf 4, (not . multOf) 100]
                  where
                    multOf :: Int -> Bool
                    multOf = (== 0) . mod i

nextDate :: Date -> Date
nextDate date = Date { dayOfWeek = next $ dayOfWeek date, dayOfMonth = dm', year = y', month = m' }
  where
    nextMonth :: Bool
    nextMonth = intDm == (daysInMonth (month date) (year date))
    intDm = (intValue . dayOfMonth) date
    dm' = if nextMonth then DayOfMonth 1 else DayOfMonth (intDm + 1)
    m' = ((if nextMonth then next else id) . month) date
    isNextYear :: Bool
    isNextYear = and [nextMonth, month date == end]
    y' = ((if isNextYear then nextYear else id) . year) date

basis :: Date
basis = Date { dayOfWeek = Monday, dayOfMonth = DayOfMonth 1, year = Year 1900, month = Jan }

foreverDates :: [Date]
foreverDates = iterate nextDate basis

numSundays = length $ takeWhile ((< Year 2001) . year) $ filter ((== Sunday) . dayOfWeek) foreverDates


main :: IO ()
main = putStrLn $ "Number of Sundays in 20th Century: " ++ (show numSundays)
