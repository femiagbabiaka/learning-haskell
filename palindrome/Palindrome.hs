module Palindrome where

import Data.Char

isPalindrome :: String -> Maybe Bool
isPalindrome w =
  let
    notSpace :: Char -> Bool
    notSpace x = not (x == ' ')
    word = (map toLower (filter isAlpha w))
  in
  case word of
    [] -> Nothing
    _ -> Just (word == reverse word)
