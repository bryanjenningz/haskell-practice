toDigits :: Integer -> [Integer]
toDigits n
  | n >= 10 = toDigits (n `div` 10) ++ [(n `mod` 10)]
  | n >= 1 = [n]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = everyOther (*2)

everyOther :: (a -> a) -> [a] -> [a]
everyOther f (x:y:zs) = x : f y : everyOther f zs
everyOther f xs = xs

flatten :: [[a]] -> [a]
flatten (xs:ys) = xs ++ flatten ys
flatten [] = []

sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ flatten $ map toDigits xs

-- Uses checksum algorithm to validate a credit card number.
validate :: Integer -> Bool
validate n = (sumDigits $ doubleEveryOther $ toDigitsRev n) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)
-- Moves peg a to peg b in 2^n - 1 steps
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n > 1 = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a
  | n == 1 = [(a, b)]
  | otherwise = []
