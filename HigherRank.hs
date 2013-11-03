import Data.STRef
import Control.Monad.ST.Safe
import Control.Monad (replicateM_)

main = (putStrLn . show) stResult

stResult = runST $ do ref <- newSTRef 0
                      replicateM_ 1000000 $ modifySTRef' ref (+1)
                      readSTRef ref

--- Can't be done!
--- v  = runST (newSTRef "abc")

foo :: (forall a. a -> a) -> (Char,Bool)
foo f = (f 'c', f True)

bar :: forall a. ((a -> a) -> (Char, Bool))
bar f = ('c', True)


