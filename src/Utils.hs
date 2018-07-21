--
-- Utils.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Utils where
  --(
  --) where


order :: (Ord a) => a -> a -> (a,a)
order x y = (min x y, max x y)

rangeExclusive :: (Ord a, Enum a) => a -> a -> [a]
rangeExclusive x y = 
  let
    (mn,mx) = order x y
  in
    enumFromTo (succ mn) (pred mx)


