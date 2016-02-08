module Euler89 (Numeral (..), minimalNumerals, parseNumerals, numeralValue) where

--import qualified Data.Map as M (Map, fromList, lookup, elems)
import qualified Data.List as L (sortBy)

data Numeral = I | V | X | L | C | D | M deriving (Show)

data Piece = Single Numeral | Subtractive Numeral Numeral deriving (Show)

valueOfPiece :: Piece -> Int
valueOfPiece (Single n) = numeralValue n
valueOfPiece (Subtractive s b) = (numeralValue b) - (numeralValue s)

allowedSubtractives :: [(Numeral, Numeral)]
allowedSubtractives  = [(I, V), (I, X), (X, L), (X, C), (C, D), (C, M)]

allowedNumerals :: [Numeral]
allowedNumerals = [I, V, X, L, C, D, M]

allowedPieces :: [Piece]
allowedPieces = (map (\t -> Subtractive (fst t) (snd t)) allowedSubtractives) ++ (map Single allowedNumerals)

allowedPiecesSorted :: [Piece]
allowedPiecesSorted = L.sortBy (\p1 p2 -> compare (valueOfPiece p2) (valueOfPiece p1)) allowedPieces

unPiece :: Piece -> [Numeral]
unPiece (Subtractive i j) = [i,j]
unPiece (Single n) = [n]

minimalNumerals :: Int -> [Numeral] 
minimalNumerals = concatMap unPiece . minimalPieces

minimalPieces:: Int -> [Piece]
minimalPieces n | n <= 0 = []
                | otherwise = reverse $ go n allowedPiecesSorted []
                  where go 0 _ accum = accum
                        go _ [] accum = accum
                        go m (p:ps) accum = let v = valueOfPiece p in
                                              case (v `compare` m) of
                                                LT -> go (m - v) (p:ps) (p:accum)
                                                EQ -> go (m - v) (p:ps) (p:accum)
                                                GT -> go m ps accum



parseNumerals :: String -> Maybe [Numeral]
parseNumerals = mapM charMapping


charMapping :: Char -> Maybe Numeral
charMapping 'I' = Just I
charMapping 'V' = Just V
charMapping 'X' = Just X
charMapping 'L' = Just L
charMapping 'C' = Just C
charMapping 'D' = Just D
charMapping 'M' = Just M
charMapping  _  = Nothing

numeralValue :: Numeral -> Int
numeralValue I = 1
numeralValue V = 5
numeralValue X = 10
numeralValue L = 50
numeralValue C = 100
numeralValue D = 500
numeralValue M = 1000




