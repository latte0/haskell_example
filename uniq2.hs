main = do
 cs <- getContents
 putStr $ unlines $ uniq $ lines cs
uniq :: [String] -> [String]
uniq [] = []
uniq (x:xs) = addlist [x] xs
 where
  match :: [String] -> String -> Bool
  match [] pattern = False
  match (x:xs) pattern = if x == pattern then True else match xs pattern

  addlist :: [String] -> [String] -> [String]
  addlist c [] = c
  addlist c (x:xs) = if match c x then addlist c xs else addlist (c ++ [x]) xs


  {-
前回の課題
  main = do
   cs <- getContents
   putStr $ unlines $ oddList $ lines cs

  shippo :: [a] -> [a]
  shippo [] = []
  shippo (x:xs) = xs

  oddList :: [a] -> [a]
  oddList [] = []
  oddList (x:xs) = [x] ++ shippo xs

  evenList :: [a] -> [a]
  evenList [] = []
  evenList (x:xs) = oddList xs
  -}
