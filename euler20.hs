factorial :: (Integral a) => a -> a
factorial = foldr (*) 1 . enumFromTo 1

sumDigits :: (Integral a) => a -> a
sumDigits = sumIter 0
    where sumIter acc 0 = acc
          sumIter acc n = sumIter ((+) acc $ n `rem` 10) $ n `div` 10
              
main = print $ sumDigits $ factorial 100