module CsSpring13.Week3.Golf where
import Data.List

skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips xs = reverse $ skipHelper (length xs) xs

skipHelper 1 xs = [xs]
skipHelper n xs = map fst (filter (\(x,y) -> y `mod` n == 0) $ zip xs [1..]) : skipHelper (n-1) xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs
            | length xs <= 2 = []
            | otherwise = maximaHelper (head xs) (tail xs)

maximaHelper :: Integer -> [Integer] -> [Integer]
maximaHelper n [] = []
maximaHelper n [x] = []
maximaHelper n (x:xs)
                | x > n && x > head xs = x : maximaHelper x xs
                | otherwise = maximaHelper x xs

histogram :: [Integer] -> String
histogram xs = histogramHelper gs "" ++ "==========\n0123456789\n"
        where
            gs = map (\x -> (head x, length x)) $ group $ sort xs

histogramHelper :: [(Integer, Int)] -> String -> String
histogramHelper [] ls = ls
histogramHelper xs ls = histogramHelper rs nl
    where
        ys = map fst xs
        rs = [(x, y-1) | (x,y) <- xs ,y > 1]
        nl = encodeLine ys ++ "\n" ++ ls

encodeLine :: [Integer] -> String
encodeLine xs = map (\x -> if x `elem` xs then '*' else ' ')[0..9]
