module ReverseTest where

import Test.QuickCheck

evalLengthTake :: Int -> [a] -> Bool
evalLengthTake n xs = length (take n xs) == n

prop_lengthTakeSafe :: Property
prop_lengthTakeSafe =
  forAll
    (listOf (arbitrary :: Gen Int))
    (\xs -> forAll (choose (0, length xs)) (\n -> evalLengthTake n xs))

prop_lengthTake :: Property
prop_lengthTake =
  forAll
    (listOf (arbitrary :: Gen Int))
    (\xs -> forAll (arbitrary :: Gen Int) (\n -> evalLengthTake n xs))

testLengthTake :: IO ()
testLengthTake = do
  quickCheck prop_lengthTakeSafe
  quickCheck prop_lengthTake
