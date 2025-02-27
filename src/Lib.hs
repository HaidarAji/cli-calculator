module Lib
    ( evaluate
    ) where

import Text.Read (readMaybe)

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a 
safeDiv _ 0 = Nothing
safeDiv x y = Just ( x / y )

evaluate :: String -> Either String Double
evaluate input =
    case words input of
        [a, op, b] -> case (readMaybe a, readMaybe b) of
            (Just x, Just y) -> case op of
                "+" -> Right $ (+) x y
                "-" -> Right $ (-) x y
                "*" -> Right $ (*) x y
                "/" -> case safeDiv x y of
                    Just z -> Right z
                    Nothing -> Left "Error: Division by zero"
                _ -> Left "Error: Unsupported operator"
            _ -> Left "Error: Invalid numbers"
        _ -> Left "Error: Invalid input format"
            