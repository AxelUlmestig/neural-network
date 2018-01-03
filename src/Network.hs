
module Network (
    initializeNetwork,
    query,
    feedForward,
    Network,
    Layer,
    WeightedInput,
    Activation
) where

import Data.Matrix
import System.Random

import ActivationFunction

type WeightedInput = Matrix Double
type Activation = Matrix Double

type Layer = (Matrix Double, Matrix Double)
type Network = [Layer]

query :: Network -> Activation -> Matrix Double
query ann = snd . last . feedForward' ann

{-
evaluateLayer :: Matrix Double -> Layer -> Matrix Double
evaluateLayer input (weights, biases) = activationFunc <$> (weights * input + biases)
-}

feedForward :: Network -> Activation -> ([WeightedInput], [Activation])
feedForward ann = unzip . feedForward' ann

feedForward' :: Network -> Activation -> [(WeightedInput, Activation)]
feedForward' [] _                = []
feedForward' ((w,b):ann) input   = (z, a) : feedForward' ann a
    where   z = w * input + b
            a = activationFunc <$> z

initializeNetwork :: RandomGen g => g -> [Int] -> Network
initializeNetwork seed (currentLayer:nextLayer:layers) =
    (weights, biases) : initializeNetwork nextSeed (nextLayer:layers)
    where   (randomNumbers, nextSeed)   = randomizeArray seed (currentLayer * nextLayer)
            weights                     = fromList nextLayer currentLayer randomNumbers
            biases                      = fromList nextLayer 1 $ repeat 0
initializeNetwork _ _ = []

randomizeArray :: RandomGen g => g -> Int -> ([Double], g)
randomizeArray seed n | n <= 0  = ([], seed)
randomizeArray seed n           = (x : responseArray, responseSeed)
    where   (x, nextSeed)                   = randomR (-0.5, 0.5) seed
            (responseArray, responseSeed)   = randomizeArray nextSeed (n - 1)
