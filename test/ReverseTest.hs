module ReverseTest where

import Control.Applicative (liftA2)
import Test.QuickCheck

(.==.) = liftA2 (==)

reverseIdentity = (reverse . reverse) .==. id

genIntList :: Gen [Int]
genIntList = arbitrary `suchThat` (\x -> length x > 3)

prop_reverse_identity :: Property
prop_reverse_identity = forAll genIntList (\n -> reverseIdentity n)

main :: IO ()
main = do
  quickCheck prop_reverse_identity
