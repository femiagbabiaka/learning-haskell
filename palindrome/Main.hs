module Main where

import Palindrome

map' :: (a -> a) -> [a] -> [a]
map' apply list =
  case list of
    [] -> []
    (first : rest) -> (apply first) : (map' apply rest)

head' :: [a] -> a
head' (first : rest) =
  first

tail' :: [a] -> [a]
tail' xs =
  case xs of
    [] -> []
    (first : rest) -> rest

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred xs =
  case xs of
    [] -> []
    (x:xs) ->
      case pred x of
        True -> x : filter' pred xs
        False -> filter' pred xs

main :: IO ()
main = do
  word <- getLine
  case (isPalindrome word) of
    Nothing -> print "Sorry, this is an empty string."
    Just False -> print "Sorry, this is not a palindrome."
    Just True -> print "Nice, this is a palindrome!"
