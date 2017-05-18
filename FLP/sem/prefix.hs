isPrefix :: String -> String -> Bool
isPrefix _ [] = True
isPrefix [] _ = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

notPrefix :: String -> String -> Bool
notPrefix x y = not (isPrefix x y)
