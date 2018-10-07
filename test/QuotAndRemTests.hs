module SumTest where

import Test.QuickCheck

genTwoNum :: Gen (Int, Int)
genTwoNum = arbitrary `suchThat` (\(x, y) -> x /= 0 && y /= 0)

quotAndRemRelation x y = (quot x y) * y + (rem x y) == x

divAndModRelation x y = (div x y) * y + (mod x y) == x

prop_quot_and_rem :: Property
prop_quot_and_rem = forAll genTwoNum (\(x, y) -> quotAndRemRelation x y)

prop_div_and_mod :: Property
prop_div_and_mod = forAll genTwoNum (\(x, y) -> divAndModRelation x y)

main :: IO ()
main = do
  quickCheck prop_quot_and_rem
  quickCheck prop_div_and_mod
