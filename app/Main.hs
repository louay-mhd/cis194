module Main where

import Lib
import PowerX
import Text.Printf

main :: IO ()
main = do
    print (factorial 4)
    printf "%.4f\n" (xpower 0.5)
    printf "%.4f\n" (xpower 5.0)
    someFunc
