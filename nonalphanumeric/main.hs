module Main where

import Data.Char (isAlpha)

rejectNonAlphabetic :: String -> Maybe String
rejectNonAlphabetic string =
  case (myAll isAlpha string) of
    False -> Nothing
    True -> Just string

myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = foldr (\x y -> pred x && y) True
-- myAll _ [] = True
-- myAll pred (x:xs) =
--   case (pred x) of
--     False -> False
--     True -> myAll pred xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs) =
  case (pred x) of
    True -> True
    False -> myAny pred xs

-- foldr' takes a "binary function", a function that takes two arguments and returns one result, an acc (often the identity or zero value), or start value, and a list, to fold over
-- foldr' should be thought as the "reduce" function from other langs
foldr' :: (a -> b -> b) -> b -> [a] -> b
-- default case so recursion can end
foldr' f acc [] = acc
-- this works similarly to map, but without cons'ing (list concatenation). we recurse over the list, applying our f function to each item, and accumulating the result in acc
foldr' f acc (x:xs) = f x (foldr f acc xs)

main :: IO ()
main = do
  print "Hello, world!"
