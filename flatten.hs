data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]

flatten (Elem x) = [x]
flatten (List []) = []
flatten (List ((NestedList x) : xs)) = flatten (NestedList x) : flatten xs
