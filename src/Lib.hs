module Lib
    ( evaluate
    , evaluate'
    ) where

import Text.Read (readMaybe)

safeDiv :: (Eq a, Fractional a) => a -> a -> Maybe a 
safeDiv _ 0 = Nothing
safeDiv x y = Just ( x / y )

operation :: Fractional a => String -> Maybe ( a -> a -> a )
operation "+" = Just (+)
operation "-" = Just (-)
operation "*" = Just (*)
operation "/" = Just (/)
operation  _ = Nothing

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

evaluate' :: String -> Either String Double
evaluate' input =
    case words input of
        [a, op, b] -> let x = readMaybe a
                          y = readMaybe b in case operation op <*> x <*> y of
                                                Just z -> Right z
                                                Nothing -> Left "Error: Invalid numbers or operator"
        _ -> Left "Error: Invalid input format"
                