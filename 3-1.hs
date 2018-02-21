main = do
 cs <- getContents
 putStr $ unlines $ map addTab $ lines cs

addTab :: String -> String
addTab cs = cs ++ "¥t"
