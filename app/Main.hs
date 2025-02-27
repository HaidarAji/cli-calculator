module Main (main) where

import Lib ( evaluate
            , evaluate'
            )

main :: IO ()
main = do
    mapM_ putStrLn greeting
    loop

greeting :: [String]
greeting = [
        "Welcome to Haskell CLI Calculator"
        , "Enter an expression (or type 'exit' to quit)"
    ]

loop :: IO ()
loop = do 
    putStr "> "
    -- hFlush stdout
    input <- getLine
    if input == "exit"
        then putStrLn "Goodbye!"
        else do
            case evaluate' input of
                Right x -> putStrLn $ "Result: " ++ show x
                Left y -> putStrLn y
            loop
