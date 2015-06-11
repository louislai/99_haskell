data ListItem a = Single a | Multiple Int a
  deriving Show
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified c = case c of
    [] -> []
    a:rest -> (encode $ takeWhile (== a) c) : (encodeModified $ dropWhile (== a) c)

encode :: Eq a => [a] -> ListItem a
encode a = case (length a) of
    1 -> Single (head a)
    l -> Multiple l (head a)
