data ListItem a = Single a | Multiple Int a
  deriving Show
encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect c = case c of
    [] -> []
    a:rest -> (encode $ takeWhile (== a) c) : (encodeDirect $ dropWhile (== a) c)

encode :: Eq a => [a] -> ListItem a
encode a = case (length a) of
    1 -> Single (head a)
    l -> Multiple l (head a)
