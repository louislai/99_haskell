dupli :: [a] -> [a]
dupli s = case s of
    [] -> []
    a:rest -> (replicate 2 a) ++ (dupli rest)
