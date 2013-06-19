import Math.NumberTheory.Primes.Factorisation (factorise')

euler3 :: Integer -> Integer
euler3 n = maximum $ map fst $ factorise' $ n

main = print $ euler3 600851475143