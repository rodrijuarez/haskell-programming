module Half where

import Test.QuickCheck

genDivisor :: Gen Float
genDivisor = arbitrary `suchThat` (/= 0)

half x = x / 2

halfIdentity = (* 2) . half

prop_half :: Property
prop_half = forAll genDivisor (\n -> half n * 2 == n)

prop_half_identity :: Property
prop_half_identity = forAll genDivisor (\n -> halfIdentity n == n)

main :: IO ()
main = do
  quickCheck prop_half
  quickCheck prop_half_identity
