module Main where
import System

main = do putStrLn "The arguments are:"
           av <- getArgs
           putStr $ unlines av
