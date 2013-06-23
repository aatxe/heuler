factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * (factorial $ n - 1)

lattice :: (Integral a) => a -> a
lattice n = (factorial $ 2 * n) `div` (factorial n)^2

main = print $ lattice 20