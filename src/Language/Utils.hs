module Language.Utils where

import Prelude (error)
import Protolude

-- | `splitOn (e==) (xs ++ [e] ++ ys) == (xs, e, ys)` splits on the first occurrence
splitOn :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitOn f = go []
  where
    go _ [] = Nothing
    go acc (x : xs)
      | f x = Just (reverse acc, x, xs)
      | otherwise = go (x : acc) xs

unreachablePattern :: a
unreachablePattern = error "unreachablePattern has been reached"
