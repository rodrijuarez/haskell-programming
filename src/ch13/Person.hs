module Person where

import System.IO

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
    PersonInvalidUnknown $ "Name was: " ++ show name ++ "Agewas:" ++ show age

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please insert your name: "
  name <- getLine
  putStrLn "Please insert your age: "
  age <- getLine
  case mkPerson name (read age :: Integer) of
    (Left NameEmpty) -> putStrLn "Empty Name"
    (Left AgeTooLow) -> putStrLn "Age too low"
    (Left (PersonInvalidUnknown error)) -> putStrLn error
    (Right person) ->
      putStrLn $ "Yay! Successfully got a person: " ++ (show person)
