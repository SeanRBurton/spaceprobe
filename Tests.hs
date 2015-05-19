import Control.Exception (assert)
import Control.SpaceProbe.Probe
import Control.SpaceProbe.Search
import Data.Maybe (isJust)
import Test.QuickCheck

import Debug.Trace
import Data.Tree

validateSearchTree :: SearchTree t -> Bool
validateSearchTree (SearchTree node xs)
  = assert (not allExplored || mu <= m) $
    assert (not $ isNaN mu) $
    assert (not $ isNaN m) $
    assert (minimum [n, fromIntegral k, fromIntegral k'] >= 0) $
    assert (fromIntegral k' <= n) $
    assert (fromIntegral k == length xs) $
    assert (fromIntegral k' == length explored) $
    assert (n == sum playouts + if isJust x && n /= 0 then 1 else 0) $
    assert (m >= maximum maxes) $
    --assert (not allExplored || (mu <= mu_max && mu >= mu_min)) $
    if n == 0 
      then True 
      else assert (all validateSearchTree xs)
    True 
  where SearchNode x mu m n k k' = node
        nan = 0 / 0
        nodes = map _node xs
        fullyExplored (SearchNode _ _ _ n k k') = k == k' && (n /= 0)
        explored = filter fullyExplored nodes
        means = map _mean nodes
        maxes = map _maximum nodes
        playouts = map _playouts nodes
        allExplored = all (>0) playouts
        mu_max = maximum means
        mu_min = minimum means
  
maximizationTree :: (t -> Float) -> Probe t -> Int -> SearchTree t
maximizationTree eval p k = go k inf (-inf) $ searchTree p
  where inf = 1 / 0 :: Float                                                    
        go 0 _ _ t = t
        go k l u t = if b then t' else go (k - 1) l' u' t'
          where PlayoutResult t' x e l' u' b = playout eval l u t    

data FloatEval = FloatEval String (Float -> Float)

instance Show FloatEval where
  show (FloatEval s _) = s

floatEvals :: [FloatEval]
floatEvals = [
  FloatEval "square" $ \x -> x ** 2,
  FloatEval "poly" $ \x -> x ** 3 + 5 * x ** 2 + 0.01 * x ** 7,
  FloatEval "case" $ \x -> if x < 2 then x else x + 4,
  FloatEval "sin" sin,
  FloatEval "floor log" $ \x -> (x - fromIntegral (floor x)) + 5 + log (abs x),
  FloatEval "sub" $ \x -> x - 2,
  FloatEval "exp" x,
  FloatEval "const" $ \x -> 0, 
  FloatEval "sqrt" $ \x -> abs x ** 0.5]

instance Arbitrary FloatEval where
  arbitrary = oneof $ map return floatEvals

validProbe p eval k = validateSearchTree $ maximizationTree eval p k

data DefaultProbe t = Exponential t
                    | Normal t t
                    | Uniform t t deriving (Show)

makeFloatingProbe (Exponential mu) = exponential mu
makeFloatingProbe (Normal mu sigma) = normal mu sigma
makeFloatingProbe (Uniform a b) = uniform a b

arbitraryExponential :: (Arbitrary t, Num t) => Gen (DefaultProbe t)
arbitraryExponential = 
  do mu <- arbitrary
     return . Exponential $ abs (abs mu + 1)

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

test1 p (FloatEval _ eval) (NonNegative k) =
  validProbe (makeFloatingProbe p) eval (abs k)

searchTreeToTree (SearchTree n xs) = Data.Tree.Node n $ map searchTreeToTree xs 
                                                                                
fmtTree t k = drawTree $ fmap show $ cut k $ searchTreeToTree $ t               
                                                                                
cut 0 (Data.Tree.Node n xs) = Data.Tree.Node n []                               
cut k (Data.Tree.Node n xs) = Data.Tree.Node n $ map (cut $ k - 1) xs           
                                                                                
layer 0 (Data.Tree.Node n _) = [n]                                              
layer k (Data.Tree.Node _ xs) = concatMap (layer $ k - 1) xs                    
                                                                                
                      
