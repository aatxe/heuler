euler2 :: Int -> Int
euler2 n = sum $ filter (\x -> x `rem` 2 == 0) $ fib' n
    where fib' n = takeWhile (< n) $ map fib [1..]
	
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main :: IO ()
main = print $ euler2 4000000