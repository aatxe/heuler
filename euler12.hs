divisors :: Int -> [Int]
divisors n = 1 : filter ((== 0) . rem n) [2..n]

countDivisors :: Int -> Int
countDivisors n = length $ divisors n

triangle :: Int -> Int
triangle n = sum [1..n]
	
euler12 :: Int -> Int
euler12 n = head $ filter divisorCheck $ map triangle [1..]
  where divisorCheck x = countDivisors x > n

main :: IO ()
main = print $ euler12 500
