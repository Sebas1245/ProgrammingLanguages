succesor :: Integer -> Integer
succesor x = x + 1

sumSquares :: Integer -> Integer -> Integer
sumSquares x y = x * x + y * y

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

posint :: Int -> [Int] -> Int
posint _ [] = error "Integer not found"
posint x (y:list) = 
    if x == y then 1
    else 1 + posint x list

car :: [a] -> a
car xs = case xs of
    [] -> error "Empty list!"
    (x:_) -> x