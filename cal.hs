import System.Environment

main = do
 args <- getArgs
 putStrLn $ calday (read $ args !! 0) (read $ args !! 1) ((read $ args !! 2) + (read $ args !! 3))
calday :: Int -> Int -> Int -> String
calday year month day = loopcalc year month day

uruu :: Int -> Bool
uruu y = if ((mod y 4 == 0) && (mod y 100 /= 0)) || (mod y 400 == 0)then True else False


getmonthday :: Int -> Int -> Int
getmonthday y m = if (uruu y) then [31, 29 ,31,30,31,30,31,31,30,31,30,31] !! (m-1) else [31, 28 ,31,30,31,30,31,31,30,31,30,31] !! (m-1)


{- [y m nokori] -}

addyear :: Int -> Int -> Int
addyear y m = if m == 12 then y + 1 else y

calcmonth :: Int -> Int
calcmonth m = if m == 12 then 1 else m + 1

loopcalc :: Int -> Int -> Int -> String
loopcalc y m d = if (d > getmonthday y m) then loopcalc (addyear y m) (calcmonth m) (d - getmonthday y m) else "" ++ show y ++ show m ++ show d
