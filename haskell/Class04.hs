signCount :: [Int] -> (Int, Int)
signCount [] = (0,0)
signCount (f:r)
    | f == 0 = (n,p)
    | f < 0 = (n + 1, p)
    | otherwise = (n, p + 1)
    where (n, p) = signCount r

split :: Int -> [Int] -> ([Int], [Int])
split thresh lst = 
    let lt = [ x| x <- lst, x < thresh]
        gt = [ x| x <- lst, x >= thresh]
    in (lt, gt)