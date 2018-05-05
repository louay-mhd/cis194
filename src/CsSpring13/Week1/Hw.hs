module CsSpring13.Week1.Hw where
{- card number validation-}
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | n < 10 = [n]
    | otherwise = toDigits(n `quot` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
{--
doubleEveryOther xs = helper xs False
helper :: [Integer] -> Bool -> [Integer]
helper [] _ = []
helper ys b
    | b = helper xs (not b) ++ [x*2]
    | otherwise = helper xs (not b) ++ [x]
    where
        x  = last ys
        xs = init ys
-}

doubleEveryOther xs = reverse (map (\(x,y) -> if even x then y*2 else y) (zip [1..] (reverse xs)))

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map sum digits)
    where digits = map toDigits xs

validate :: Integer -> Bool
validate n = x `mod` 10 == 0
    where
        x = sumDigits (doubleEveryOther (toDigits n))

{- hanoi problem -}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ (a, b) : hanoi (n-1) c b a
