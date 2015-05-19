{-# LANGUAGE FlexibleInstances #-}

import Control.Exception (assert)
import Control.Monad.Identity (Identity(..), runIdentity)
import Control.SpaceProbe
import Control.SpaceProbe.Internal.Optimize
import Data.List (sort)
import Data.Maybe (isJust)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

validateSearchTree :: SearchTree t -> Bool
validateSearchTree (SearchTree node xs)
  = assert (n == 0 || mu <= m) $
    assert (not $ isNaN mu) $
    assert (not $ isNaN m) $
    assert (minimum [n, fromIntegral k, fromIntegral k'] >= 0) $
    assert (fromIntegral k' <= n) $
    assert (fromIntegral k == length xs) $
    assert (fromIntegral k' == length explored) $
    assert (n == sum playouts + if isJust x && n /= 0 then 1 else 0) $
    assert (null xs || m >= maximum maxes) $
    if n == 0 
      then True 
      else assert (all validateSearchTree xs)
    True 
  where SearchNode x mu m n k k' = node
        nodes = map _node xs
        fullyExplored (SearchNode _ _ _ n_ k_ k'_) = k_ == k'_ && (n_ /= 0)
        explored = filter fullyExplored nodes
        maxes = map _maximum nodes
        playouts = map _playouts nodes

playout :: (t -> Float) -> Float -> Float -> SearchTree t -> PlayoutResult t
playout eval l u = runIdentity . playoutM (Identity . eval) l u

maximizationTree :: (t -> Float) -> Probe t -> Int -> SearchTree t
maximizationTree eval p k = go k inf (-inf) $ searchTree p
  where inf = 1 / 0 :: Float                                                    
        go 0 _ _ t = t
        go k' l u t
          | b || (u' == inf) = t'
          | otherwise = go (k' - 1) l' u' t'
          where PlayoutResult t' _ _ l' u' b = playout eval l u t    

data Eval a = Eval String (a -> Float)

instance Show (Eval a) where
  show (Eval s _) = s

floatEvals :: [Eval Float]
floatEvals = [
  Eval "square" $ \x -> x ** 2,
  Eval "poly" $ \x -> x ** 3 + 5 * x ** 2 + 0.01 * x ** 7,
  Eval "case" $ \x -> if x < 2 then x else x + 4,
  Eval "sin" sin,
  Eval "floor log" $ \x -> (x - fromIntegral (floor x :: Integer)) + 5 + 
                                log (2 + abs x),
  Eval "sub" $ \x -> x - 2,
  Eval "exp" exp,
  Eval "const" $ const 0, 
  Eval "sqrt" $ \x -> abs x ** 0.5]

intEvals :: [Eval Int]
intEvals = [Eval s (f . fromIntegral) | Eval s f <- floatEvals]

instance Arbitrary (Eval Float) where
  arbitrary = oneof $ map return floatEvals

instance Arbitrary (Eval Int) where
  arbitrary = oneof $ map return intEvals

validProbe :: Probe t -> (t -> Float) -> Int -> Bool
validProbe p eval k = validateSearchTree $ maximizationTree eval p k

data DefaultProbe t = Exponential t | Normal t t
                    | Uniform t t deriving (Show)

makeFloatProbe :: DefaultProbe Float -> Probe Float 
makeFloatProbe (Exponential mu) = exponential mu
makeFloatProbe (Normal mu sigma) = normal mu sigma
makeFloatProbe (Uniform a b) = uniform a b

makeIntProbe :: DefaultProbe Int -> Probe Int
makeIntProbe (Exponential mu) = exponentialInt (fromIntegral mu)
makeIntProbe (Normal mu sigma) = 
  normalInt (fromIntegral mu) $ fromIntegral sigma 
makeIntProbe (Uniform a b) = uniformInt a b

arbitraryExponential :: (Arbitrary t, Num t, Ord t) => Gen (DefaultProbe t)
arbitraryExponential = 
  do (NonNegative mu) <- arbitrary
     return $ Exponential mu 

arbitraryNormal :: (Arbitrary t, Num t, Ord t) => Gen (DefaultProbe t)
arbitraryNormal = 
  do mu <- arbitrary
     (NonNegative sigma) <- arbitrary
     return $ Normal mu sigma 

arbitraryUniform :: (Arbitrary t, Num t, Ord t) => Gen (DefaultProbe t)
arbitraryUniform = 
  do a <- arbitrary
     (NonNegative x) <- arbitrary
     return $ Uniform a x 

instance (Arbitrary t, Num t, Ord t) => Arbitrary (DefaultProbe t) where
  arbitrary = oneof [arbitraryExponential, arbitraryNormal, arbitraryUniform]

prop_ValidFloat :: DefaultProbe Float -> Eval Float -> NonNegative Int -> Bool
prop_ValidFloat p (Eval _ eval) (NonNegative k) =
  validProbe (makeFloatProbe p) eval k

prop_ValidInt :: DefaultProbe Int -> Eval Int -> NonNegative Int -> Bool
prop_ValidInt p (Eval _ eval) (NonNegative k) = 
  validProbe (makeIntProbe p) eval k

prop_ExponentialIntBounds :: NonNegative Float -> Positive Int -> Bool
prop_ExponentialIntBounds (NonNegative mu) (Positive k) = 
  all (>=(0 :: Int)) .
  take k .
  map fst .
  maximize (const 0) $
  exponentialInt mu

prop_UniformIntExhaustive :: Int -> Positive Int -> Property
prop_UniformIntExhaustive a (Positive x) = 
  sort (map fst . maximize (const 0) $ uniformInt a b) === [a..b-1]
  where b = a + x

prop_UniformBounds :: Float -> Positive Float -> Positive Int -> Bool
prop_UniformBounds a (Positive x) (Positive k) = 
 all (\y -> a <= y && y < b) . 
 take k . 
 map fst . 
 maximize (const 0) $ 
 uniform a b
 where b = a + x

prop_ExponentialBounds :: NonNegative Float -> Positive Int -> Bool
prop_ExponentialBounds (NonNegative mu) (Positive k) = 
  all (>=0) .
  take k . 
  map fst .
  maximize (const 0) $
  exponential mu 

strictMonotone :: Ord a => [a] -> Bool
strictMonotone [] = True
strictMonotone xs = all id $ zipWith (>) xs (tail xs)  

prop_LowestYet :: [(Int, Float)] -> Bool
prop_LowestYet = strictMonotone . map snd . lowestYet

prop_HighestYet :: [(Int, Float)] -> Bool
prop_HighestYet = strictMonotone . reverse . map snd . highestYet

tests :: [Test]
tests = [testProperty "prop_ValidFloat" prop_ValidFloat,
         testProperty "prop_ValidInt" prop_ValidInt,
         testProperty "prop_UniformIntExhaustive" prop_UniformIntExhaustive,
         testProperty "prop_UniformBounds" prop_UniformBounds,
         testProperty "prop_ExponentialBounds" prop_ExponentialBounds,
         testProperty "prop_LowestYet" prop_LowestYet,
         testProperty "prop_HighestYet" prop_HighestYet,
         testProperty "prop_ExponentialIntBounds" prop_ExponentialIntBounds] 

main :: IO ()
main = defaultMain tests
