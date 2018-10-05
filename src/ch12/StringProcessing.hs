module StringProcessing where

import Data.Maybe (fromMaybe)

-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a") . map notThe . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel =
  snd .
  foldr
    (\x y ->
       case (x, y) of
         (Nothing, (Just (x:xs), acc)) ->
           if (x `elem` "aeiou")
             then (Nothing, acc + 1)
             else (Nothing, acc)
         (x, (_, acc)) -> (x, acc))
    (Just "", 0) .
  map notThe . words

countVowels :: String -> Integer
countVowels =
  foldr
    (\x y ->
       if (x `elem` "aeiou")
         then y + 1
         else y)
    0

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord xs =
  if (wordLength - numberOfVowels < numberOfVowels)
    then Just (Word' xs)
    else Nothing
  where
    wordLength = length xs
    numberOfVowels = fromInteger $ countVowels xs
