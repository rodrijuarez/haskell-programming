module MultTest where

import Test.QuickCheck

multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative x y = x * y == y * x

genThreeNum :: Gen (Int, Int, Int)
genThreeNum = arbitrary

genTwoNum :: Gen (Int, Int)
genTwoNum = arbitrary

prop_associative :: Property
prop_associative = forAll genThreeNum (\(x, y, z) -> multAssociative x y z)

prop_commutative :: Property
prop_commutative = forAll genTwoNum (\(x, y) -> multCommutative x y)

main :: IO ()
main = do
  quickCheck prop_associative
  quickCheck prop_commutative
