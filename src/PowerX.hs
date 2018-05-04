module PowerX
    ( factorial,
      xpower
    ) where

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

term :: Double -> Integer -> Double
term x 0 = 1.0
term x 1 = x
term x n = x ** fromIntegral n / fromIntegral (factorial n)

xpower :: Double -> Double
xpower x = sum (map term [0..9])
    where
        term 0 = 1.0
        term n = x ** fromIntegral n / fromIntegral (f n)
        f 0 = 1
        f 1 = 1
        f n = n * f (n-1)

f :: [Int] -> [Int] -> Double -> Double
f a b x = sum (map (\(aa, bb) -> fromIntegral aa * x ** fromIntegral bb) (zip a b))
curve :: Int -> Int -> [Int] -> [Int] -> [Double]
curve l r a b = map (f a b) [fromIntegral l,fromIntegral l + 0.001..fromIntegral r]
