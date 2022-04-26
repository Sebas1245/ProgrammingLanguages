average :: [(Integer, String, [Int])] -> [(Integer, Int)]
average [] = []
average ((id, _, grades):rest) = (id, sum grades `div` length grades):average rest

data BT e = N (BT e) e (BT e) | ET deriving (Show)

leaves:: BT a -> [a]
leaves ET = []
leaves (N ET root ET) = [root]
leaves (N leftST _ rightST) = leaves leftST ++ leaves rightST