{-# LANGUAGE BangPatterns #-}
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

module Control.SpaceProbe.Search (
  PlayoutResult(..),
  SearchNode(..),
  SearchTree(..), 
  playout,
  searchTree,
  -- * Optimization
  maximize,
  minimize,
  -- * Optimization output processing
  highestYet,
  lowestYet,
  evaluateForusecs
) where 

import Control.SpaceProbe.Probe
import Control.Exception (evaluate)
import Data.Int (Int16, Int64)
import Data.Tree (Tree(..))
import System.Clock (getTime, Clock(Monotonic), TimeSpec(..))
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Timeout (timeout)

data SearchNode t = SearchNode {
  _value            :: t,
  _mean             :: !Float,
  _maximum          :: !Float,
  _playouts         :: !Int64,
  _numchildren      :: !Int16,
  _exploredChildren :: !Int16
} deriving (Show)

data SearchTree t = SearchTree {
  _node     :: !(SearchNode (Maybe t)),
  _children :: ![SearchTree t]
} deriving (Show)

searchTree :: Probe t -> SearchTree t
searchTree (Probe x f r) = go0 x
  where newNode y  n = SearchNode y 0 (-inf) 0 n 0 where inf = 1 / 0
        newTree v xs = 
          SearchTree (newNode v (fromIntegral $ length xs)) xs
        makeTree s = newTree $ r s
        go0 s = makeTree s . map go1 $ f s 
        go1 t = case t of
                  Node s []  -> go0 s 
                  Node s ys  -> makeTree s $ map go1 ys 

i2f :: Integral a => a -> Float
i2f = fromIntegral

update :: SearchNode t -> Float -> Bool -> (SearchNode t, Bool)
update (SearchNode x mu m n k k') e b = 
  (SearchNode x mu' m' n' k k'', k'' == k)
  where n' = n + 1
        m' = max m e
        mu' = mu + (e - mu) / i2f n'
        k'' = if b then k' + 1 else k'

ucb :: Int64 -> Float -> Float -> SearchNode t -> Float
ucb n_total l u (SearchNode _ mu m n k k')
  | n  == 0 = -1.0 / 0
  | k' == k = 1
  | otherwise = negate $ normalize (0.8 * mu + 0.2 * m) + 
                         sqrt (2 * log (i2f n_total) / i2f n)
  where normalize x
          | u == l = -0.5
          | otherwise = (x - l) / (u - l)

insertOn :: Ord b => (a -> b) -> a -> [a] -> [a]
insertOn f x = go
  where go [] = [x]
        go ys@(y:ys') 
          | f x > f y = y : go ys'
          | otherwise = x : ys

insert :: Float -> 
          Float -> 
          Int64 ->
          (SearchTree a) -> 
          [SearchTree a] -> 
          [SearchTree a]
insert l u n = insertOn (ucb (n + 1) l u . _node)

data PlayoutResult t = PlayoutResult {
  _tree          :: !(SearchTree t),
  _input         :: !t,
  _eval          :: !Float,
  _min           :: !Float,
  _max           :: !Float,
  _fullyExplored :: !Bool
}

playout :: (t -> Float) -> 
           Float -> 
           Float ->
           SearchTree t -> 
           PlayoutResult t
playout eval = go 
  where go !l !u (SearchTree !a !xs) =
          case (_value a, xs) of
            (Nothing, []) -> error "playout"
            (Just x,   _) -> if null xs || _playouts a == 0
                               then let  e = eval x
                                         (a', b) = update a e False
                                    in PlayoutResult {
                                         _tree          = 
                                           SearchTree a' xs,
                                         _input         = x,
                                         _eval          = e,
                                         _min           = min l e,
                                         _max           = max u e,
                                         _fullyExplored = b 
                                       }
                               else recur 
            (Nothing, _) -> recur 
          where recur = let (y:ys) = xs
                            r =  go l u y
                            zs = insert (_min r) 
                                        (_max r) 
                                        (_playouts a) 
                                        (_tree r) ys
                            (a', b) = update a (_eval r) (_fullyExplored r)
                        in r{_tree = SearchTree a' zs, _fullyExplored = b}      

-- | Fairly self-explanatory. Maximize the objective function @eval@ over the
-- given @Probe@. Keeps lazily generating samples until it explores the whole
-- search space so you almost certainly want to apply some cut-off criterion. 

maximize :: (t -> Float) -> Probe t -> [(t, Float)]
maximize eval p = go inf (-inf) $ searchTree p
  where inf = 1 / 0 :: Float 
        go l u t = (x, e) : (if b || (u' == inf) then [] else go l' u' t')
          where PlayoutResult t' x e l' u' b = playout eval l u t

-- | The opposite of maximize. 
-- 
-- @minimize eval = map (fmap negate) . maximize (negate . eval)@
minimize :: (t -> Float) -> Probe t -> [(t, Float)]
minimize eval = map (fmap negate) . maximize (negate . eval)

highestYet_ :: Ord b => (a -> b) -> [a] -> [a]
highestYet_ _ [] = []
highestYet_ f (x:xs) = x : go (f x) xs
  where go _ [] = []
        go k (y:ys)
          | m > k = y : go m ys
          | otherwise = go k ys
          where m = f y

lowestYet_ :: (Num b, Ord b) => (a -> b) -> [a] -> [a]
lowestYet_ f = highestYet_ (negate . f)

-- | Preserves only those elements @(_, b)@ for which @b@ is higher than for
-- all previous previous values in the list. Designed for use with maximize
highestYet ::[(a, Float)] -> [(a, Float)]
highestYet = highestYet_ snd

-- | Preserves only those elements @(_, b)@ for which @b@ is lower than for all
-- previous values in the list. Designed for use with minimize.
-- 
-- @lowestYet xs == map (fmap negate) . highestYet . map (fmap negate)@
lowestYet :: [(a, Float)] -> [(a, Float)]
lowestYet = lowestYet_ snd

getTimeInUsecs :: IO Int64                                                      
getTimeInUsecs = do TimeSpec s n <- getTime Monotonic                           
                    return $ 1000000 * s + n `div` 1000                         

-- | Take the largest prefix of @xs@ which can be evaluated 
-- within @dt@ microseconds.
evaluateForusecs :: Int -> [a] -> IO [a]                                                 
evaluateForusecs dt xs = do{t <- getTimeInUsecs; go t xs}                                
  where go !_ ![]     = return []                                               
        go  t  (x:ys) =                                                         
          do t' <- getTimeInUsecs                                               
             m  <- timeout (fromIntegral . max 0 $ t + fromIntegral dt - t') $  
                   evaluate x                                                   
             case m of                                                          
               Nothing -> return []                                             
               Just y  -> do zs <- unsafeInterleaveIO $ go t ys
                             return $ y:zs

