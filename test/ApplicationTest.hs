module ApplicationTest where

import Control.Applicative (liftA2)
import Test.QuickCheck

sumOne = (+ 1)

sumTwo = (+ 2)

genInt :: Gen Int
genInt = arbitrary

application :: (Eq b) => (a -> b) -> a -> Bool
application f a = (f $ a) == f a

composeApplication :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
composeApplication f g x = (f . g) x == (\x -> f (g x)) x

prop_application :: Property
prop_application = forAll genInt (\n -> application sumOne n)

prop_composition :: Property
prop_composition = forAll genInt (\n -> composeApplication sumOne sumTwo n)

main :: IO ()
main = do
  quickCheck prop_application
  quickCheck prop_composition
