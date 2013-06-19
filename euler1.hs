import Data.List (union)

euler1 :: Int -> Int
euler1 n = sum $ mult3 `union` mult5
	where mult3 = filter (\x -> x `rem` 3 == 0) [1..(n - 1)]
	      mult5 = filter (\x -> x `rem` 5 == 0) [1..(n - 1)]

main = print $ euler1 1000