module Laws.Monoid
  ( monoidLeftIdentity
  , monoidRightIdentity
  ) where

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity m = mempty `mappend` m == m

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity m = m `mappend` mempty == m
