import Data.Char (isAlpha)

database :: [(Integer, String)]
database = [(1, "Femi"),
            (2, "Alaina"),
            (3, "Frankie"),
            (4, "Melman")]


greetUser :: Integer -> Maybe String
greetUser record =
  fmap ("Hello, " ++) ((lookup record database) >>= makeUsername)

rejectNonAlphabetic :: String -> Maybe String
rejectNonAlphabetic string =
  case (all isAlpha string) of
    False -> Nothing
    True -> Just string

removeSpaces :: String -> Maybe String
removeSpaces string =
  case (filter (\x -> not (x == ' ')) string) of
    "" -> Nothing
    result -> Just result

validateLength :: String -> Maybe String
validateLength string =
  case (length string > 25) of
    True -> Nothing
    False -> Just string

makeUsername :: String -> Maybe String
makeUsername string =
  removeSpaces string >>= rejectNonAlphabetic >>= validateLength


main :: IO ()
main = do
  word <- getLine
  print (isPalindrome word)

isPalindrome :: String -> Bool
isPalindrome string =
  string == reverse string
