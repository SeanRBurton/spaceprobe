{-# LANGUAGE ExistentialQuantification #-}

-- |                                                                            
-- Module : Control.SpaceProbe.Probe                                           
-- Copyright : Sean Burton 2015                                            
-- License : BSD3                                                               
--                                                                              
-- Maintainer : burton.seanr@gmail.com                                          
-- Stability : experimental                                                     
-- Portability : unknown                                                        
--                                                                              
-- An applicative combinator library for parameter optimization designed        
-- to perform well over high-dimensional and/or discontinuous search spaces,    
-- using Monte-Carlo Tree Search with several enhancements.    

module Control.SpaceProbe.Internal.Probe where

import Control.Applicative (
  Alternative, 
  Applicative, 
  empty,
  pure, 
  (<*>), 
  (<|>),
  (<$>))
import Data.Number.Erf (InvErf, invnormcdf, normcdf)
import Data.Tree(Forest, Tree(..))

-- | The main data structure for this module; it describes 
--  a search space and an associated exploration strategy.
-- 
-- This type is an instance of the following classes: 
-- 
-- * 'Functor' which does the obvious thing.
-- 
-- * 'Applicative', which allows us to combine multiple search spaces and 
--   optimize over them simultaneously.
--
-- * 'Alternative', which allows us to optimize over the disjoint union of 
--   two search spaces.
data Probe t = forall s . Probe {
  _initial :: s, 
  -- ^ The initial search space.
  _partition :: s -> Forest s,
  -- ^ A function to partition a given search space and
  -- remove its representative from contention
  _draw :: s -> Maybe t
  -- ^ Try to choose a 'representative element' from the search space.
  -- For example, if the search space were the interval [0, 10),
  -- a suitable representative might be the midpoint 5. After the 
  -- initial search space has been recursively partitioned as deeply as
  -- possible, every possible element should be the representative of exactly 
  -- one subspace.
}

-- | generate a partition function to be use in the construction of custom
-- Probes. 
newPartition :: (s -> [s]) -> (s -> Forest s)
newPartition f = map (\x -> Node x []) . f

tipConcatMap :: (a -> Forest a) -> Tree a -> Tree a
tipConcatMap f (Node x []) = Node x $ f x
tipConcatMap f (Node x xs) = Node x $
  case xs of 
    [] -> f x
    _  -> map (tipConcatMap f) xs

instance Functor Probe where
  fmap g (Probe x f d) = Probe x f $ fmap g . d 

instance Applicative Probe where
  pure x = Probe x (const []) (Just . id)
  (Probe x0 f0 d0) <*> (Probe x1 f1 d1) = Probe (x0, x1) f d
    where f (s0, s1) = map (tipConcatMap (t1 . fst)) $ t0 s1 
            where t0 s1' = [do {s0' <- t; return (s0', s1')} | t <- f0 s0]
                  t1 s0' = [do {s1' <- t; return (s0', s1')} | t <- f1 s1]
          d (s0, s1) = d0 s0 <*> d1 s1 

instance Alternative Probe where
  empty = Probe [] (const []) undefined
  (Probe x0 f0 d0) <|> (Probe x1 f1 d1) = 
    Probe {
      _initial    = Nothing,
      _partition  = partition, 
      _draw       = draw
    } where partition Nothing = map (flip Node [] . Just) [Left x0, Right x1]
            partition (Just (Left  x)) = map (fmap $ Just . Left)  $ f0 x
            partition (Just (Right x)) = map (fmap $ Just . Right) $ f1 x
            draw m = m >>= either d0 d1

ave :: Num a => (a -> a -> a) -> a -> a -> a
ave divide a b = a + (b - a) `divide` 2

floatAve :: Floating a => a -> a -> a
floatAve = ave (/)

intAve :: Integral a => a -> a -> a
intAve = ave quot

-- | Uses inverse transform sampling to draw from a probability distribution 
-- given the associated inverse cumulative distribution function.
distribution :: Floating b => (b -> a) ->  Probe a
distribution invcdf = invcdf <$> uniform 0 1 

-- | Sample from the exponential distribution with given mean. Useful for
-- constants which are potentially unbounded but probably small.
exponential :: Floating a => a -> Probe a 
exponential mu = distribution $ \x -> mu * log(1 / (1 - x))  

-- | Sample from the normal distribution with given mean and standard 
-- deviation
normal :: (Eq a, InvErf a) => a -> a -> Probe a
normal mu sigma = (\x -> x * sigma + mu) <$> distribution invnormcdf

-- | Sample uniformly from the interval [a, b). 
uniform :: Floating a => a -> a -> Probe a 
uniform a b = Probe {
  _initial   = (a, floatAve a b, b),
  _partition = newPartition $
                 \(a', x, b') -> [(a', floatAve a' x, x), 
                                  (x,  floatAve x b', b')],
  _draw = \(_, x, _) -> Just x
}

bisect :: (Integral a, Num b, Ord b) => (a -> b) -> b -> a -> a -> a
bisect f y a b = go (intAve a b) a b
  where go u a' b'
          | b' <= a' = u
          | otherwise = let v    = intAve a' b' 
                            z    = f v
                        in case compare z y of 
                             LT -> go v (v + 1) b' 
                             EQ -> v 
                             GT -> go v a' v

-- | Approximately sample from a probability distribution over the range
-- [a, b). Relies on splitting the range into regions of approximately 
-- equal probability so will be less accurate for small ranges 
-- or highly unequal distributions.
intDistribution :: (Integral a, Floating b, Ord b) => 
                   (a -> b) ->
                    a -> 
                    a ->
                    Probe a
intDistribution cdf a b = Probe {
  _initial = (a, mid a b, b),
  _partition = newPartition partition,
  _draw = \(_, x, _) -> Just x
} where mid a' b' = bisect cdf (floatAve (cdf a') (cdf b')) a' b'
        partition (a', x, b') = 
          filter (\(u, _, v) -> v > u) 
            [(a', mid a' x, x),
             (x + 1, mid (x + 1) b', b')]

exponentialInt :: (Bounded a, Integral a) => Float -> Probe a
exponentialInt mu = 
  intDistribution (\x -> 1 - exp (-fromIntegral x / mu)) 0 maxBound

-- | Sample from an approximate normal distribution with given mean and 
-- standard deviation. May fail if a very large mean and/or standard deviation 
-- is given.
normalInt :: (Bounded a, Integral a) => Float -> Float -> Probe a
normalInt mu sigma = 
  intDistribution (\x -> normcdf $ (fromIntegral x - mu) / sigma) 
                  (round (mu - bound) + 1)
                  (round (mu + bound) - 1)  
  where bound = 6 * sigma

-- | Sample uniformly from the interval [a, b).
uniformInt :: (Eq a, Integral a) => a -> a -> Probe a
uniformInt a b = Probe {
  _initial = (a, intAve a b, b),
  _partition = newPartition partition',
  _draw = \(_, x, _) -> Just x
} where partition' (a', x, b') =
          filter (\(u, _, v) -> v > u) [(a',     intAve a' x, x),
                                        (x + 1,  intAve (x + 1) b', b')]

-- | Choose from a list of constants with equal probability.
constants :: [a] -> Probe a
constants xs = Probe Nothing partition id
  where partition Nothing = map (flip Node [] . Just) xs
        partition _ = []

permutation :: [Integer] -> [a] -> Integer -> [a]
permutation _  [] _ = []
permutation [] _  _ = error "permutation: factorial list too short"
permutation (u:facs) xs n = y : permutation facs ys r
  where (q, r) =  quotRem n u
        (y:ys) = let (us, v:vs) = splitAt (fromIntegral q) xs 
                 in  v:us ++ vs 

-- | Samples uniformly from permutations of @xs@. Makes the assumption that 
-- permutations which are lexicographically close are likely to have similar
-- fitness.
permute :: [a] -> Probe [a]
permute xs = permutation facs xs <$> uniformInt 0 u 
  where (u:facs) = 
          reverse $ scanl (*) 1 [1..fromIntegral $ length xs] :: [Integer]

extractElem :: [a] -> [(a, [a])]
extractElem [] = []
extractElem (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (extractElem xs)

-- | Samples sublists of @xs@ of size @k@. The order of elements in @xs@ is 
-- irrelevant.
sizedSublist :: Int -> [a] -> Probe [a]
sizedSublist k xs = Probe {
  _initial = (0, xs, []),
  _partition = newPartition $ 
                  \(n, xs', ys) -> 
                    if n == k 
                      then [] 
                      else [(n + 1, zs, x:ys) | (x, zs) <- extractElem xs'],
  _draw = \(m, xs', ys) -> Just $ ys ++ take (k - m) xs'  
}

-- | Samples sublists of @xs@ of size @k@ with replacement. 
-- The order of elements in @xs@ is irrelevant.
sizedWithReplacement :: Int -> [a] -> Probe [a]
sizedWithReplacement k xs = Probe {
  _initial = (0, []),
  _partition = newPartition $ \(n, ys) -> if n == k 
                                            then [] 
                                            else [(n + 1, x:ys) | x <- xs],
  _draw = Just . snd 
}

-- | Samples progressively larger sublists of @xs@. More 'important' elements
-- (those which are likely to affect the fitness of a sample more) should
-- ideally be placed closest to the start of @xs@.
sublist :: [a] -> Probe [a]
sublist xs = Probe {
  _initial    = (xs, []),
  _partition = newPartition $ \(xs', ys) ->
                  case xs' of 
                    [] -> []
                    (x:xs'') -> [(xs'', x:ys), (xs'', ys)],
  _draw       = Just . reverse . snd 
}

-- | Samples progressively larger sublists of @xs@ with replacement. The order
-- of elements in @xs@ is irrelevant.
withReplacement :: [a] -> Probe [a]
withReplacement xs = Probe {
  _initial = [],
  _partition = newPartition $ \ys -> map (:ys) xs,
  _draw = Just . id
}
