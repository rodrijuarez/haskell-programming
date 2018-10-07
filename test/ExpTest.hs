module ExpTest where

import Test.QuickCheck

expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

expCommutative x y = x ^ y == y ^ x

genThreeNum :: Gen (Int, Int, Int)
genThreeNum = arbitrary

genTwoNum :: Gen (Int, Int)
genTwoNum = arbitrary

prop_associative :: Property
prop_associative = forAll genThreeNum (\(x, y, z) -> expAssociative x y z)

prop_commutative :: Property
prop_commutative = forAll genTwoNum (\(x, y) -> expCommutative x y)

main :: IO ()
main = do
  quickCheck prop_associative
  quickCheck prop_commutative
