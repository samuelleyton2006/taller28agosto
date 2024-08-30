maxList :: [Int] -> Int
maxList [] = error "lista vacía"
maxList [x] = x
maxList (x:xs) = max x (maxList xs)