module Phone where

import Data.Char (isUpper, toLower)
import Data.List (elemIndex, find, maximumBy)
import Data.Ord (comparing)

type Key = (Digit, [Char])

data DaPhone =
  DaPhone [Key]
  deriving (Show)

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone letter
  | isUpper letter = ('*', 1) : reverseTaps phone (toLower letter)
  | otherwise = [(fst phoneKey, index + 1)]
  where
    phoneKey = findLetter phone letter
    index = letterIndex letter phoneKey

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone letters = foldr ((++) . reverseTaps phone) [] letters

findLetter :: DaPhone -> Char -> Key
findLetter (DaPhone layout) letter =
  case find (\(_, chars) -> letter `elem` chars) layout of
    (Just phoneKey) -> phoneKey
    _ -> head layout

letterIndex :: Char -> Key -> Int
letterIndex letter key =
  case letter `elemIndex` (snd key) of
    (Just n) -> n
    Nothing -> 0

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter = maximumBy (comparing (fingerTaps . reverseTaps oldPhone))

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . foldr ((:) . mostPopularLetter) ""

coolestWord :: [String] -> String
coolestWord = undefined

oldPhone :: DaPhone
oldPhone =
  DaPhone
    [ ('1', "")
    , ('2', "abc")
    , ('3', "def")
    , ('4', "ghi")
    , ('5', "jkl")
    , ('6', "mno")
    , ('7', "pqrs")
    , ('8', "tuv")
    , ('9', "wxyz")
    , ('*', "^")
    , ('0', "+_")
    , ('#', ".,")
    ]
