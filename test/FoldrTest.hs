module FoldrTest where

import Control.Applicative (liftA2)
import Test.QuickCheck

(.==.) :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
(.==.) = liftA2 (==)

genIntList :: Gen [Int]
genIntList = arbitrary `suchThat` (\x -> length x > 3)

genNestedIntList :: Gen [[Int]]
genNestedIntList = arbitrary `suchThat` (\x -> length x > 3)

spineEquality = foldr (:) [] .==. ([] ++)

concatenationEquality = foldr (++) [] .==. concat

prop_spine_equality :: Property
prop_spine_equality = forAll genNestedIntList (\n -> spineEquality n)

prop_concatenation_equality :: Property
prop_concatenation_equality =
  forAll genNestedIntList (\n -> concatenationEquality n)

main :: IO ()
main = do
  quickCheck prop_concatenation_equality
  quickCheck prop_spine_equality
