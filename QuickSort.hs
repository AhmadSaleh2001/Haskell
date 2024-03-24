qsort :: [Int] -> [Int]
qsort [] = []
qsort (x : xs) = (qsort a) ++ [x] ++ (qsort b)
 where
 a = [a | a <- xs, a <= x]
 b = [b | b <- xs, b > x]