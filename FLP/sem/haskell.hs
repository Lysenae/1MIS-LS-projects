getPrintable :: [String] -> [String]
getPrintable [] = []
getPrintable (x:xs)
  | xs == []  = [x]
  | xs /= [] && (notPrefix x (head xs)) = x:(getPrintable2 xs x)
  | otherwise = getPrintable2 xs x

getPrintable2 :: [String] -> String -> [String]
getPrintable2 [] _ = []
getPrintable2 (x:xs) prev
  | xs == [] && (notPrefix x prev) = [x]
  | xs /= [] && (notPrefix x prev) && (notPrefix x (head xs)) = x:(getPrintable2 xs x)
  | otherwise = getPrintable2 xs x

isPrefix :: String -> String -> Bool
isPrefix _ [] = True
isPrefix [] _ = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

notPrefix :: String -> String -> Bool
notPrefix x y = not (isPrefix x y)
