factorial :: (Integral a) => a -> a
factorial = foldr (*) 1 . enumFromTo 1

lattice :: (Integral a) => a -> a
lattice n = (factorial $ 2 * n) `div` (factorial n)^2

main :: IO ()
main = print $ lattice 20