module Lib where

import Data.Char

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Tree = Node {left:: Tree, right:: Tree, value:: Int} | NilTree
    deriving (Show)


sumTree :: Tree -> Int
sumTree NilTree = 0
sumTree node = value node + sumTree (left node) + sumTree (right node)


addNodeLeft:: Tree -> Tree -> Tree
addNodeLeft node NilTree = node
addNodeLeft node parent = Node {value = value parent, left = node, right = right parent}

addNodeRight:: Tree -> Tree -> Tree
addNodeRight node NilTree = node
addNodeRight node parent = Node {value = value parent, right = node, left = left parent}

node :: Int -> Tree
node x = Node {value = x, left = NilTree, right = NilTree}


strong :: String -> Bool
strong s
       | length s < 15 = False
       | otherwise = any isUpper s &&  any isDigit s && any isLower s
