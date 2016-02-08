{-# LANGUAGE BangPatterns #-}

module Util (foldLines, transformLines) where

import System.IO

transformLines :: (String -> String) -> (IO ())
transformLines f = do iseof <- isEOF
                      if iseof
                        then return ()
                        else getLine >>= putStrLn . f >> transformLines f

foldLines :: (a -> String -> a) -> a -> (IO a)
foldLines f a = do iseof <- isEOF
                   if iseof
                     then return a
                     else getLine >>= (\s -> foldLines f (sf a s))
                       where
                         sf !a' !s' = f a' s'
