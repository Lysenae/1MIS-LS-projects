data Person = Person { name :: String
                     , surname :: String
                     , id :: Integer
                     }
    deriving (Show)

data BST
  = Empty
  | Node Person BST BST
  deriving (Show)

readData :: FilePath -> IO [String]
readData file = do
  contents <- readFile file
  return (lines contents)
  
parseData :: [String] -> BST -> BST
parseData [] b = b
parseData (x:xs) b = parseData xs (insert (Person n s (read i)) b) where
  (n,r1) = span (/=':') x
  (s,r2) = span (/=':') (tail r1)
  i      = tail r2

insert :: Person -> BST -> BST
insert p Empty = Node p Empty Empty
insert p@(Person n s i) (Node o@(Person on os oi) l r)
  | s > os = Node o l (insert p r)
  | s < os = Node o (insert p l) r
  | n > on = Node o l (insert p r)
  | n < on = Node o (insert p l) r
  | i > oi = Node o l (insert p r)
  | i < oi = Node o (insert p l) r
  | otherwise = Node p l r

main = do
  c <- readData "data"
  print (parseData c Empty)
