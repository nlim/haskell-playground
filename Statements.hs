import Control.Monad.Trans.State.Lazy


tick :: State Int String
tick =  return "foo"

tick2 :: State Int String
tick2 = do s <- tick
           return $ s ++ "bar"

tick3 = do s <- tick
           s2 <- tick2
           s3 <- tick2
           put 1600
           return $ s3

type Stack = [Int]

pop :: State Stack Int
pop = do (x:xs) <- get
         put xs
         return x

push :: Int -> State Stack ()
push a = do xs <- get
            put (a:xs)
            return ()

stackManip = do push 3
                push 3
                push 3
                i <- pop
                push $ i*29
                pop


