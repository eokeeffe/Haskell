flatten::[[a]]->[a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs