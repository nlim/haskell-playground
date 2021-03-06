module Weird where

import Data.List (sortBy, foldl')
import Control.Monad.State

head' :: [a] -> Maybe a
head' [] = Nothing
head' (a:_) = Just a

getMin  :: [(Bool, Int)] -> Maybe (Bool, Int)
getMin  vs = head' $ sortBy (\i j -> compare (snd i) (snd j)) $ filter fst vs

getMin2 :: [(Bool, Int)] -> Maybe Int
getMin2 vs = foldl' r Nothing vs where
    r (Just i) (b,j) = case (b, compare j i) of
                          (True, LT) -> Just j
                          _ -> Just i
    r Nothing (b,j)  = case b of
                          True  -> Just j
                          False -> Nothing

data Stuff = Foo | Bar | Baz deriving (Show, Eq)

toStr :: Stuff -> String
toStr Foo = "foo"
toStr Bar = "bar"
toStr Baz = "baz"

mapToStr :: [Stuff] -> [String]
mapToStr = map toStr

weirdF :: Stuff -> Stuff -> Stuff
weirdF a b = if a == b then Foo else Baz


data Amount s v = Amount s v

emitThreshold :: (Ord s, Integral v) => v -> v -> [Amount s v] -> [(s, v)]
emitThreshold starting threshold amounts = concat $ evalState (mapM emitThreshold' amounts) $ starting where
  emitThreshold' (Amount s v) = do
    modify $ (+v)
    balance <- get
    case (balance `div` threshold) of
      n | n > (fromInteger 0) -> (modify $ \b -> b - thresholds) >> return [(s, thresholds)] where
        thresholds = n * threshold
      _                       -> return []


