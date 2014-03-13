module Util (foldLines) where

import System.IO

{-# LANGUAGE BangPatterns #-}

foldLines :: (a -> String -> a) -> a -> (IO a)
foldLines f a = do iseof <- isEOF
                   if iseof
                     then return a
                     else getLine >>= (\s -> foldLines f (sf a s))
                       where
                         sf !a' !s' = f a' s'
