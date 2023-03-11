toDigits :: Integer -> [Integer]
toDigits x | x <= 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x <= 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | even (length xs) =  reverse $ go $ (reverse xs)
  | otherwise = go xs
  where
    go []       = []
    go (x:[])   = [x]
    go (x:y:xs) = x : (y+y) : go xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x >= 10 = sum (toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs

validate n = case (go n) of
  0 -> True
  _ -> False
  where
    go n = (rem) (seq n) 10
    seq = sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c  =
  hanoi (n-1) a c b ++
  hanoi 1     a b c ++
  hanoi (n-1) c b a
