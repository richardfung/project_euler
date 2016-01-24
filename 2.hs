main = do
    print $ sum $ filter (\x -> x `mod` 2 == 0) $ takeWhile (<= 4 * 10^6) fibs

fibs :: [Int]
fibs = let h a b = let next = a+b
                   in next:(h b next)
       in 1:2:(h 1 2)
