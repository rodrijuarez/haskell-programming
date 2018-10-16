module Lib
  ( someFunc
  ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

myreplicateM 0 _ = return []
myreplicateM n m = (:) <$> m <*> (myreplicateM (n - 1) m)
