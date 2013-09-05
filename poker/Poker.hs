import Data.List.Split

--- Data Types
newtype Rank = Rank { getRank :: Int } deriving (Show)

data Suit = Diamonds | Hearts | Spades | Clubs deriving (Show)

data Card = Card Suit Rank deriving (Show)

data Hand = Hand Card Card Card Card Card deriving (Show)

data Try a = Failure String | Success a deriving (Show)

instance Monad Try where
  (Success a) >>= f = f a
  (Failure s) >>= f = Failure s
  return a = Success a;

--- Creation Functions
makeRank :: Int -> Try Rank

makeRank n
  | n >= 1 && n <= 13 = Success (Rank n)
  | otherwise = Failure ("Rank integer: " ++ (show n) ++ " is not between 1..13")


makeSuit :: Char -> Try Suit
makeSuit 'C' =  Success Clubs
makeSuit 'D' = Success Diamonds
makeSuit 'H' = Success Hearts
makeSuit 'S' = Success Spades
makeSuit c = Failure ("Suit Char: " ++ [c] ++ " is not C or D or H or S")

makeCard :: String -> Try Card
makeCard s = do
  (suitChar, rankS) <- parsePieces s
  suit <- makeSuit suitChar
  rank <- makeRank $ parseInt rankS
  return (Card suit rank)

pokerHand :: [Card] -> Try Hand
pokerHand (a:b:c:d:e:[]) = Success (Hand a b c d e)
pokerHand list = Failure ("List of Cards: " ++ (show list) ++ " is not 5 cards long")

--- Parsing Functions
parsePieces :: String -> Try (Char, String)
parsePieces s =
    case splitAt 1 s of
      (h:[], j) -> Success (h, j)
      otherwise -> Failure ("String: " ++ s ++ " could not be parse into pieces")


parseInt :: String -> Int
parseInt = read

parseHand :: String -> Try Hand
parseHand s = do
  cards <- mapM makeCard (splitOn " " s)
  hand <- pokerHand cards
  return hand

--- Main
main :: IO ()
main = do
  putStrLn "Input Hand: e.g. C1, C2, H3, C4, D5"
  line <- getLine
  putStrLn (show (parseHand line))
