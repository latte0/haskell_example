import System.Environment
import Data.List

main = do
 args <- getArgs
 cs <- getContents
 putStr $ fgrep (head args) cs

fgrep :: String -> String -> String
fgrep pattern cs = unlines $ filter match $ lines ( unlines $ map format $ zipLineNumber $ lines cs )
  where
   match :: String -> Bool
   match line = any prefixp $ tails line

   prefixp :: String -> Bool
   prefixp line = pattern `isPrefixOf` (getback 5 line)

   zipLineNumber :: [String] -> [(Int, String)]
   zipLineNumber xs = zip [1..] xs

   format :: (Int, String) -> String
   format (n, line) = rjust 4 (show n) ++ " " ++ line

   rjust :: Int -> String -> String
   rjust width s = replicate (width - length s) ' ' ++ s

   getback :: Int -> String -> String
   getback i N[] = []
   getback i gch = if i == 0 then gch else (getback (i-1) (tail gch))
