dropEvery :: [a] -> Int -> [a]
dropEvery a n = helper a n n
    where
        helper z _ 0 = z
        helper [] _ _ = []
        helper (b:rest) 1 l = helper rest l l
        helper (b:rest) ct l = b : (helper rest (ct - 1) l)
