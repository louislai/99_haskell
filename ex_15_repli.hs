repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ 0 = []
repli (a:rest) n = (replicate n a) ++ (repli rest n)
