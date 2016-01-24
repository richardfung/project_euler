import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad (forM_, when)

main = do
    let n = 600851475143
    print $ head $ reverse $ filter (\x -> n `mod` x == 0)
            $ primes $ floor $ sqrt $ fromIntegral n

primes :: Int -> [Int]
primes n = 
    let bools = runSTUArray $ do
        ns <- newArray (2, n) True
        forM_ [2..n] $ \i -> do
            v <- readArray ns i
            when v $ forM_ [i+i, i+i+i..n] $ \j -> writeArray ns j False
            return ()
        return ns
    in map fst $ filter (\(_,b) -> b) $ assocs bools
