module SumTest where

import Test.QuickCheck

plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative x y = x + y == y + x

genThreeNum :: Gen (Int, Int, Int)
genThreeNum = arbitrary

genTwoNum :: Gen (Int, Int)
genTwoNum = arbitrary

prop_associative :: Property
prop_associative = forAll genThreeNum (\(x, y, z) -> plusAssociative x y z)

prop_commutative :: Property
prop_commutative = forAll genTwoNum (\(x, y) -> plusCommutative x y)

main :: IO ()
main = do
  quickCheck prop_associative
  quickCheck prop_commutative
