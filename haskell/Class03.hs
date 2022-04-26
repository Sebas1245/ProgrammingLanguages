sumabs :: [Integer] -> Integer
sumabs = sum . map abs

applyFns:: [a -> b -> c] -> a -> b -> [c]
applyFns bfns op1 op2 = map (\f -> f op1 op2) bfns

reduce :: (a -> b -> a) -> (b -> Bool) -> a -> [b] -> a
reduce bp pred init lst = foldl bp init (filter pred lst)

condSum::(Num a, Num b, Enum a) =>(b -> Bool) -> (a -> b) -> a -> b
condSum pred fn n = sum (filter pred (map fn [1..n]))