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

module Control.SpaceProbe (
  P.Probe(..),
  P.newPartition,
  -- * Floating search spaces
  P.distribution,
  P.exponential,
  P.normal,
  P.uniform,
  -- * Integral search spaces
  P.intDistribution,
  P.exponentialInt,
  P.normalInt,
  P.uniformInt,
  -- * Discrete search spaces
  P.constants,
  P.permute,
  P.sizedSublist,
  P.sizedWithReplacement,
  P.sublist,
  P.withReplacement,
  -- * Optimization
  O.maximize,
  O.minimize,
  -- * Monadic Optimization
  O.maximizeM,
  O.minimizeM,
  -- * Optimization in IO
  O.maximizeIO,
  O.minimizeIO,
  -- * Optimization output processing
  O.highestYet,
  O.lowestYet,
  O.evaluateForusecs
) where

import qualified Control.SpaceProbe.Internal.Optimize as O
import qualified Control.SpaceProbe.Internal.Probe as P

