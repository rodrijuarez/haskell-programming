module Sort where

import Data.List (sort)
import Test.QuickCheck

genList :: (Arbitrary a, Eq a) => Gen [a]
-- [2.1]
genList = do
  a <- arbitrary
  b <- arbitrary `suchThat` (/= a)
  arbitrary `suchThat` (`notElem` [a, b])

-- for any list you apply sort to
-- this property should hold listOrdered :: (Ord a) => [a] -> Bool listOrdered xs =
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

genListInt :: Gen [Int]
--              [2.4]
genListInt = genList

genListChar :: Gen [Char]
--              [2.5]
genListChar = genList

prop_listOrdered :: (Arbitrary a, Ord a, Show a) => Gen [a] -> Property
--    [2.6]
prop_listOrdered genList = forAll genList $ \x -> listOrdered (sort x)

prop_listOrderedInt :: Property
--    [2.7]
prop_listOrderedInt = prop_listOrdered genListInt

prop_listOrderedChar :: Property
--    [2.8]
prop_listOrderedChar = prop_listOrdered genListChar

main :: IO ()
main = do
  quickCheck prop_listOrderedInt
  quickCheck prop_listOrderedChar
