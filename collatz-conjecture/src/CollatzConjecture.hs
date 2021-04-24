module CollatzConjecture (collatz) where

getSequenceLength :: Integer -> Integer
getSequenceLength 1 = 0
getSequenceLength x = 1 + getSequenceLength (if x `mod` 2 == 1 then 3 * x + 1 else x `div` 2)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just (getSequenceLength n)
