data ListItem a = Single a | Multiple Int a
  deriving Show
decodeModified :: [ListItem a] -> [a]
decodeModified c = case c of
    [] -> []
    (Single a) : rest -> a : (decodeModified rest)
    (Multiple l a) : rest -> (replicate l a) ++ (decodeModified rest)
