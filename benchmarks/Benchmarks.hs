{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.SpaceProbe
import Criterion.Main

dsq :: Float -> Float -> Float
dsq x y = x ** 2 + y ** 2

dist :: Float -> Float -> Float
dist x y = sqrt $ dsq x y

sinsq :: Float -> Float
sinsq = (**2) . sin

cubic :: Float -> Float
cubic x = abs $ x ** 3

ackley :: (Float, Float) -> Float
ackley (x, y) = -20 * exp (-0.2 * sqrt (0.5 * dsq x y)) -
                exp(0.5 * (f x + f y)) + exp 1 + 20
  where f z = cos(2 * pi * z)

sphere :: [Float] -> Float
sphere = sum . map (**2)

rosenbrock :: [Float] -> Float
rosenbrock xs =
  sum $ zipWith (\x y -> 100 * (x - y ** 2) ** 2 + (y - 1) ** 2) (tail xs ) xs

beale :: (Float, Float) -> Float
beale (x, y) = (1.5 - x + x * y) ** 2 +
               (2.25 - x + x * y ** 2) ** 2 +
               (2.625 - x + x * y ** 3) ** 2

goldsteinPrice :: (Float, Float) -> Float
goldsteinPrice (x, y) =
  (1 + (x + y + 1) ** 2 * (19 - 14 * x + 3 * x ** 2 - 14 * y +
    6 * x * y + 3 * y ** 2)) *
  (30 + (2 * x - 3 * y) ** 2 *
    (18 - 32 * x + 12 * x ** 2 + 48 * y - 36 * x * y + 27 * y ** 2))

booth :: (Float, Float) -> Float
booth (x, y) = (x + 2 * y - 7) ** 2 + (2 * x + y - 5) ** 2

bukin :: (Float, Float) -> Float
bukin (x, y) = 100 * (sqrt . abs $ y - 0.01 * x ** 2) + 0.01 * abs (x + 10)

matyas :: (Float, Float) -> Float
matyas (x, y) = 0.26 * (x ** 2 + y ** 2) - 0.48 * x * y

levi :: (Float, Float) -> Float
levi (x, y) = sinsq(2 * pi * x) + (x - 1) ** 2 * (1 + sinsq(3 * pi * y)) +
              (y - 1) ** 2 * (1 + sinsq(2 * pi * y))

camel :: (Float, Float) -> Float
camel (x, y) = 2 * x ** 2 - 1.05 * x ** 4 + x ** 6 / 6 + x * y + y ** 2

easom :: (Float, Float) -> Float
easom (x, y) = -cos x * cos y * exp(-((x - pi) ** 2 + (y - pi) ** 2))

crossInTray :: (Float, Float) -> Float
crossInTray (x, y) = -0.0001 * (1 + abs (sin x * sin y * exp (abs u))) ** 0.1
  where u = 100 - dist x y / pi

eggHolder :: (Float, Float) -> Float
eggHolder (x, y) = -(y + 47) * sin(sqrt . abs $ y + x / 2 + 47) -
                   x * sin(sqrt . abs $ x - (y + 47))

table :: (Float, Float) -> Float
table (x, y) = negate . abs $ sin x * cos y * exp(abs $ 1 - dist x y / pi)

mcCormick :: (Float, Float) -> Float
mcCormick (x, y) = sin(x + y) + (x - y) ** 2 - 1.5 * x + 2.5 * y + 1

schafferN2 :: (Float, Float) -> Float
schafferN2 (x, y) =
  0.5 + (sinsq(x**2 - y**2) - 0.5) / (1 + 0.001 * dsq x y) ** 2

schafferN4 :: (Float, Float) -> Float
schafferN4 (x, y) =
  0.5 + (cossq(sin(abs $ x ** 2 - y ** 2)) - 0.5) / (1 + 0.001 * dsq x y) ** 2
  where cossq = (**2) . cos

styblinskiTang :: [Float] -> Float
styblinskiTang xs = sum [x ** 4 - 16 * x ** 2 + 5 * x | x <- xs] / 2

simionescu :: (Float, Float) -> Float
simionescu (x, y)
  | dsq x y <= (1 + 0.2 * cos(8 * atan(x / y))) ** 2 = 0.1 * x * y
  | otherwise = infIO
  where infIO = 1.0 / 0

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA k a =  go k (pure [])
  where go !0 !acc = acc
        go  m  acc = go (m - 1) $ (:) <$> a <*> acc

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive p xs = a ++ take 1 b
  where (a, b) = span p xs

farFrom :: Float -> Float -> Bool
farFrom 0 y = abs(y) >= 0.01
farFrom x y = abs(y - x) / abs(x) >= 0.01

runBenchmark :: Show a => (a -> Float) ->  Probe a -> Float -> IO (a, Float)
runBenchmark f p x = fmap (last . lowestYet) .
                     evaluateForusecs 700000 .
                     takeWhileInclusive (farFrom x . snd) $
                     minimize f p

square :: Float -> Float -> Probe (Float, Float)
square l u = (,) <$> uniform l u <*> uniform l u

centeredSquare :: Float -> Probe (Float, Float)
centeredSquare x = square (-x) x

perturbedSquare :: Float -> Probe (Float, Float)
perturbedSquare x = square (-x * 0.97) (x * 0.995)

perturbedHypercube :: Int -> Float -> Probe [Float]
perturbedHypercube k x = hypercube k (-x * 0.97) (x * 0.995)

hypercube :: Int -> Float -> Float -> Probe [Float]
hypercube k l u = replicateA k $ uniform l u

main :: IO ()
main = defaultMain [
  bgroup "all" [ bench "cubic" . nfIO $ runBenchmark cubic (normal 10 50) 0,
                 bench "ackley" . nfIO $
                   runBenchmark ackley (perturbedSquare 5) 0,
                 bench "sphere" . nfIO $
                   runBenchmark sphere (perturbedHypercube 10 10) 0,
                 bench "rosenbrock" . nfIO $
                   runBenchmark rosenbrock (hypercube 3 (-100) 100) 0,
                 bench "beale" . nfIO $
                   runBenchmark beale (centeredSquare 4.5) 0,
                 bench "goldsteinPrice" . nfIO $
                   runBenchmark goldsteinPrice (centeredSquare 2) 3,
                 bench "booth" . nfIO $
                   runBenchmark booth (centeredSquare 10) 0,
                 bench "bukin" . nfIO $
                   runBenchmark bukin ((,) <$> uniform (-15) (-5) <*>
                                       uniform (-3) 3) 0,
                 bench "matyas" . nfIO $
                   runBenchmark matyas (perturbedSquare 10) 0,
                 bench "levi" . nfIO $
                   runBenchmark levi (centeredSquare 10) 0,
                 bench "camel" . nfIO $
                   runBenchmark camel (centeredSquare 5) 0,
                 bench "easom" . nfIO $
                   runBenchmark easom (centeredSquare 100) (-1),
                 bench "crossInTray" . nfIO $
                   runBenchmark crossInTray (centeredSquare 10) (-2.06261),
                 bench "eggHolder" . nfIO $
                   runBenchmark eggHolder (centeredSquare 512) (-959.6407),
                 bench "table" . nfIO $
                   runBenchmark table (centeredSquare 10) (-19.2085),
                 bench "mcCormick" . nfIO $
                   runBenchmark mcCormick ((,) <$> uniform (-1.5) 4 <*>
                                           uniform (-3) 4) 0,
                 bench "schafferN2" . nfIO $
                   runBenchmark schafferN2 (perturbedSquare 100) 0,
                 bench "schafferN4" . nfIO $
                   runBenchmark schafferN4 (centeredSquare 100) 0.292579,
                 bench "styblinskiTang" . nfIO $
                   runBenchmark styblinskiTang (hypercube 5 (-5) 5)
                     (-5 * 39.16617),
                 bench "simionescu" . nfIO $
                   runBenchmark simionescu (centeredSquare 1.25) (-0.072)]]
