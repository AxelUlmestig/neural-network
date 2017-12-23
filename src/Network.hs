
module Network (
    query,
    initializeNetwork
) where

import Data.Matrix
import System.Random

type Layer = (Matrix Double, Matrix Double)
type Network = [Layer]

query :: Network -> Matrix Double -> Matrix Double
query = flip $ foldl evaluateLayer

evaluateLayer :: Matrix Double -> Layer -> Matrix Double
evaluateLayer input (weights, biases) = activationFunc <$> (weights * input + biases)

activationFunc :: Double -> Double
activationFunc = relu

relu :: Double -> Double
relu = max 0

initializeNetwork :: RandomGen g => g -> [Int] -> Network
initializeNetwork seed (currentLayer:nextLayer:layers) =
    (weights, biases) : initializeNetwork nextSeed (nextLayer:layers)
    where   (randomNumbers, nextSeed)   = randomizeArray seed (currentLayer * nextLayer + currentLayer)
            weights                     = fromList nextLayer currentLayer randomNumbers
            biases                      = fromList nextLayer 1 $ drop (currentLayer * nextLayer) randomNumbers
initializeNetwork _ _ = []

randomizeArray :: RandomGen g => g -> Int -> ([Double], g)
randomizeArray seed n | n <= 0  = ([], seed)
randomizeArray seed n           = (x : responseArray, responseSeed)
    where   (x, nextSeed)                   = randomR (0.0, 1.0) seed
            (responseArray, responseSeed)   = randomizeArray nextSeed (n - 1)
