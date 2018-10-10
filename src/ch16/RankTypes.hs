{-# LANGUAGE RankNTypes #-}

module RankTypes where

type Nat f g a = forall a. f a -> g a

maybeToList :: Nat Maybe [] a
maybeToList Nothing = []
maybeToList (Just a) = [a]

degenerateMtl :: Nat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a]
