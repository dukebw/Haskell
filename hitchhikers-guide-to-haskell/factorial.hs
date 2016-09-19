module Main where

main = do
        input <- getLine
        let result = factorial(read input :: Int)
        print result

factorial :: Int -> Int
factorial n
        | (n < 0) = error "Invalid input."
        | (n == 0) = 1
        | otherwise = n*factorial(n - 1)
