{-

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
