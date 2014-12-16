--main = putStrLn "Hello, world"

factorial :: Integer -> Integer
factorial n0 = loop 1 n0
    where loop acc n | n > 1     = loop (acc * n) (n - 1)
                     | otherwise = acc

main = factorial 10