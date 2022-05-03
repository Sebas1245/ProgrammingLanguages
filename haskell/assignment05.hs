-- Assignment 5
-- Functional programming in Haskell
-- Authors: Ana Elisa Estrada A01251091 and Estefanía Charles A01283472, Sebastián Saldaña A01570274

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

main :: IO ()
main = do
    -- Excercise 1
    print (mode 8 4 3 2 1)
    print (mode 4 2 3 2 1)
    print (mode 3 3 2 2 3)
    -- Excercise 2
    table 4