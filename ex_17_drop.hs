split :: [a] -> Int -> ([a], [a])
split a n = (take n a, drop n a)
