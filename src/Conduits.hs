import Data.Conduit
import qualified Data.Conduit.List as CL
import Control.Monad.State

--- Learning some Conduits
--- This is awesome!

--- ConduitM is a Monad Transformer, so its parameterized by the underlying
--- Monad that you are using


--- type Source m a = ConduitM () a m () -- no meaningful input or return value
--- type Conduit a m b = ConduitM a b m () -- no meaningful return value
--- type Sink a m b = ConduitM a Void m b -- no meaningful output value

source :: Source IO Int -- produces a stream of Ints
source = CL.sourceList [1..10]


source2 :: Source IO Int
source2 = do
  yield 1
  yield 2
  yield 3
  yield 4


sink :: Sink String IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String -- converts Ints into Strings
conduit = CL.map show

doublingConduit :: Conduit Int IO Int
doublingConduit = CL.map (*2)

pairWiseConduit :: Conduit Int IO (Int, Int)
pairWiseConduit = do mi <- await
                     mj <- await
                     case (mi, mj) of
                       (Just i, Just j) -> do yield (i,j)
                                              leftover j
                                              pairWiseConduit
                       _ -> return ()

--- Takes in a function over two parameters to
combinerConduit :: (a -> b -> c) -> Conduit (a,b) IO c
combinerConduit f = CL.map (uncurry f)


emitValuesAndTotals :: Conduit Int (State Int) (Int, Int)
emitValuesAndTotals = awaitForever $ \i -> do lift $ modify (+ i)
                                              x <- lift get
                                              yield (i, x)

addToWindow :: a -> Int -> [a] -> [a]
addToWindow a n l = if length l < n then a:l else a:(init l)


emitSlidingWindow :: Conduit Int (State [Int]) [Int]
emitSlidingWindow = do mi <- await
                       case mi of
                         Just i -> do lift $ modify (addToWindow i 5)
                                      x <- lift get
                                      yield x
                                      emitSlidingWindow
                         _ -> return ()

slidingWindowPipeline :: State [Int] [[Int]]
slidingWindowPipeline = CL.sourceList [1..10] $$ emitSlidingWindow =$ CL.consume

statePipeline :: State Int [(Int, Int)]
statePipeline = CL.sourceList [1..10] $$ emitValuesAndTotals =$ CL.consume

main :: IO ()
main = do print $ runState statePipeline 0
          --- source $$ pairWiseConduit =$ (combinerConduit (+) ) =$ conduit =$ sink
          --- source $$ doublingConduit =$ conduit =$ sink
