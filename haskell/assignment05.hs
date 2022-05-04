-- Assignment 5
-- Functional programming in Haskell
-- Authors: Ana Elisa Estrada A01251091 and Estefanía Charles A01283472, Sebastián Saldaña A01570274
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

mode :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
mode n1 n2 n3 n4 n5
  -- mode with freq 3
  | n1 == n2 && n1 == n3 || n1 == n3 && n2 == n4 || n1 == n4 && n1 == n5 || n1 == n2 && n1 == n5 || n1 == n3 && n1 == n5 = n1
  | n2 == n3 && n2 == n4 || n2 == n4 && n2 == n5 || n2 == n3 && n2 == n5 = n2
  | n3 == n1 && n3 == n2 || n3 == n4 && n3 == n5 = n3
  -- mode with freq 2
  | n1 == n2 || n1 == n3 || n1 == n4 || n1 == n5 = n1
  | n2 == n3 || n2 == n4 || n2 == n5 = n2
  | n3 == n4 || n3 == n5 = n3
  | n4 == n5 = n4
  -- mode with freq 1
  | otherwise = n1

table :: Integer -> IO()
table n = putStr (tableHelper n 1)


tableHelper :: Integer -> Integer -> [Char]
tableHelper n m =
    if m == 10 then
        show n ++ " x " ++ show m ++ " = " ++ show (m * n) ++ "\n"
    else
        show n ++ " x " ++ show m ++ " = " ++ show (m * n) ++ "\n" ++ tableHelper n (m + 1)

smallers :: [Integer] -> [Integer] -> [[Integer]]
smallers [] _ = []
smallers (f:rest) lst2 = findSmallersInList f lst2 : smallers rest lst2

findSmallersInList :: Integer -> [Integer] -> [Integer]
findSmallersInList n [] = []
findSmallersInList n (x:rest) =
    if x < n then
        x:findSmallersInList n rest
    else
        findSmallersInList n rest

jumps :: Integer -> Integer -> Integer -> Integer -> [Integer]
jumps n i s1 s2 =
    if n == 0 then
        []
    else
        i:jumps (n-1) (i+s1) s2 s1

-- shift :: Int -> [[Int]] -> [[Int]]
-- shift n lst = 
--     if n == 0 then 
--         lst
--     else 

-- Binary trees 
data BT t = N (BT t) t (BT t) | E deriving Show

bt :: BT Integer
bt = N (N (N E 2 E) 5 (N E 7 E)) 8 (N E 9 (N (N E 11 E) 15 E))

-- Excercise 6
list_in_order :: BT a -> String -> [a]
list_in_order bt traversalType
  | traversalType == "prefix" = prefixTraversal bt
  | traversalType == "infix" = infixTraversal bt
  | traversalType == "postfix" = postfixTraversal bt
  | otherwise = error "Traversal type not recognized!"

prefixTraversal :: BT a -> [a]
prefixTraversal E = []
prefixTraversal (N l n r) = [n] ++ prefixTraversal l ++ prefixTraversal r

infixTraversal :: BT a -> [a]
infixTraversal E = []
infixTraversal (N l n r) = infixTraversal l ++ [n] ++ infixTraversal r

postfixTraversal :: BT a -> [a]
postfixTraversal E = []
postfixTraversal (N l n r) = postfixTraversal l ++ postfixTraversal r ++ [n]

add_bst :: BT Integer -> Integer -> BT Integer
add_bst E n = N E n E
add_bst (N E root E) n =
    if n < root then
        N (N E n E) root E
    else
        N E root (N E n E)
add_bst (N E root r) n =
    if n < root then
        N (N E n E) root r
    else
        add_bst r n
add_bst (N l root E) n =
    if n < root then
        add_bst l n
    else
        N l root (N E n E)
add_bst (N l root r) n =
    if n < root then
        add_bst l n
    else
        add_bst r n

-- Higher order functions 
-- Excercise 8
g_disjoint :: Eq a => [a] -> [a] -> Bool
g_disjoint [] _ = True
g_disjoint (first:rList1) lst2 =
    notElem first lst2 &&
    g_disjoint rList1 lst2

c_evens :: Integer -> [Integer]
c_evens n =
    if n < 2 then
        []
    else
        [x*x | x <- filter even [ x | x <- [2..n]]]

f_count :: [[Int]] -> [Int]
f_count lists = map length (map (filter odd) lists)

main :: IO ()
main = do
    -- Excercise 1
    print "Excercise 1 Test Cases"
    print (mode 8 4 3 2 1)
    print (mode 4 2 3 2 1)
    print (mode 3 3 2 2 3)
    -- Excercise 2
    print "Excercise 2 Test Cases"
    table 4
    print "Excercise 3 Test Cases"
    -- Excercise 3
    print (smallers [1,2,3] [4,5,1])
    print (smallers [2,4,0] [1,2,3,4,5])
    -- Excercise 4
    print "Excercise 4 Test Cases"
    print (jumps 0 1 1 2)
    print (jumps 4 1 1 2)
    print (jumps 7 5 2 5)
    print "Excercise 6 Test Cases"
    print (list_in_order bt "prefix")
    print (list_in_order bt "infix")
    print (list_in_order bt "postfix")
    print "Excercise 7 Test Cases"
    print (add_bst E 5)
    print (add_bst (N E 5 E) 8 )
    print (add_bst (N E 5 (N E 8 E)) 2)
    print "Excercise 8 Test Cases"
    print (g_disjoint [1,2,3] [4,5,1])
    print (g_disjoint ['a', 'b', 'c'] ['d', 'e', 'f'])
    print "Excercise 9 Test Cases"
    print (c_evens 1)
    print (c_evens 10)
    print "Excercise 10 Test Cases"
    print (f_count [[1,3,5],[1,2,3]])
    print (f_count [[],[1],[1,2,3],[5,4,3,2,1]])

    