import Control.Applicative ((<$>), (<*>))

palindrome :: Int -> Bool
palindrome n = take l s == (reverse $ drop l s)
    where s = show n
          l = flip div 2 $ length s

euler4 :: Int -> Int
euler4 n = maximum $ filter palindrome $ (*) <$> [1..n] <*> [1..n]

main :: IO ()
main = print $ euler4 999