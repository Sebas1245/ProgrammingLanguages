-- Solutions to Exercise 11 problems
-- Author: Dr. Santiago Conant

-- (1) (without explicit recursion) applies a list of binary 
--     procedures to a pair of values.
apply_fns :: [a -> b -> c] -> a -> b -> [c]
apply_fns fns_lst op1 op2 = map (\f -> f op1 op2) fns_lst

-- (2) (without explicit recursion) returns the result of applying a 
--     binary procedure to all values in a list that satisfy a predicate.
reduce :: (a -> b -> a) -> (b -> Bool) -> a -> [b] -> a
reduce fn pred init lst = foldl fn init (filter pred lst)

-- (3) (without explicit recursion) returns the sum of a list of 
--     numbers that satisfy a predicate, these numbers being the 
--     result of a function applied to each of the numbers from 1 to
--     N; the predicate, the function, and the value of N are given as
--     arguments.
cond_sum :: (Num a, Num b, Enum b) => (a -> Bool) -> (b -> a) -> b -> a
cond_sum pred fn n = sum (filter pred (map fn [1..n]))

-- (4) (without explicit recursion) generates lists with the square of
--     the numbers from 1 to N, using the higher-order function until.
squares :: (Num a, Eq a) => a -> [a]
squares n = map (\x->x*x) 
                (until (\(f:_)->f==1) (\(f:r)->(f - 1):f:r) [n])

-- Testing functions
main = do
    putStrLn("Test cases for problem 1")
    putStrLn(show (apply_fns [(+),(-),(*)] 6 3))  
    -- => [9,3,18]    
    putStrLn(show (apply_fns [(<),(>),(==),(/=)] 6 3)) 
    -- => [False,True,False,True]
    putStrLn("Test cases for problem 2")
    putStrLn(show (reduce (+) odd 0 [1,2,3,4,5]))  
    -- => 9 = 1 + 3 + 5    
    putStrLn(show (reduce (*) even 1 [1,2,3,4,5]))  
    -- => 8    
    putStrLn("Test cases for problem 3")
    putStrLn(show (cond_sum (< 30) (^ 2) 100))  
    -- => 55 = 1 + 4 + 9 + 16 + 25
    putStrLn(show (cond_sum (> 0) (subtract 5) 10))  
    -- => 15
    putStrLn("Test cases for problem 4")
    putStrLn(show (squares 3))  
    -- => [1,4,9]
    putStrLn(show (squares 5))  
    -- => [1,4,9,16,25]
