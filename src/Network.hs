
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
import Data.Random.Normal

import ActivationFunction

type WeightedInput = Matrix Double
type Activation = Matrix Double

type Layer = (Matrix Double, Matrix Double)
type Network = [Layer]

query :: Network -> Activation -> Matrix Double
query ann = snd . last . feedForward' ann

feedForward :: Network -> Activation -> ([WeightedInput], [Activation])
feedForward ann = unzip . feedForward' ann

feedForward' :: Network -> Activation -> [(WeightedInput, Activation)]
feedForward' [] _                = []
feedForward' ((w,b):ann) input   = (z, a) : feedForward' ann a
    where   z = w * input + b
            a = activationFunc <$> z

initializeNetwork :: RandomGen g => g -> [Int] -> Network
initializeNetwork seed (previousLayer:currentLayer:layers) =
    (weights, biases) : initializeNetwork nextSeed (currentLayer:layers)
    where   (randomNumbers, nextSeed)   = randomizeArray seed previousLayer (previousLayer * currentLayer)
            weights                     = fromList currentLayer previousLayer randomNumbers
            biases                      = fromList currentLayer 1 $ repeat 0
initializeNetwork _ _ = []

randomizeArray :: RandomGen g => g -> Int -> Int -> ([Double], g)
randomizeArray seed previousLayer n | n <= 0  = ([], seed)
randomizeArray seed previousLayer n           = (x : responseArray, responseSeed)
    where   (x, nextSeed)                   = normal' (0.0, sqrt (1.0 / fromIntegral previousLayer)) seed
    --where   (x, nextSeed)                   = randomR (-0.5, 0.5) seed
            (responseArray, responseSeed)   = randomizeArray nextSeed previousLayer (n - 1)
