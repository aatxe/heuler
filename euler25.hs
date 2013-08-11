import Data.List  (findIndex)
import Data.Maybe (fromJust)

euler25 :: Int -> Int
euler25 n = fromJust $ findIndex (\x -> (>) x $ (^) 10 $ n - 1) fibs
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main :: IO ()
main = print $ euler25 1000