euler25 :: Int -> Int
euler25 n = length $ takeWhile (\x -> (<) x $ (^) 10 $ n - 1) fibs
  where fibs = scanl (+) 0 (1 : fibs)

main :: IO ()
main = print $ euler25 1000
