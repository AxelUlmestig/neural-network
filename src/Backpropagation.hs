
module Backpropagation (
    train
) where

import Data.Matrix

import Network
import ActivationFunction
import Util (hadamard)

type Weights = [Matrix Double]
type Biases = [Matrix Double]
type Output = Matrix Double
type Delta = Matrix Double

deltas :: Weights -> [WeightedInput] -> Output -> [Delta]
deltas weights weightedInputs expected =
    reverse $ delta : deltasInternal ws zs delta
    where   (z:zs)  = reverse weightedInputs
            ws      = reverse weights
            dCda    = costDerivative (activationFunc <$> z) expected
            delta   = dCda `hadamard` (activationFunc' <$> z)

deltasInternal :: Weights -> [WeightedInput] -> Delta -> [Delta]
deltasInternal (w:ws) (z:zs) preDelta   =
    delta : deltasInternal ws zs delta
    where   delta = (transpose w * preDelta) `hadamard` (activationFunc' <$> z)
deltasInternal _ _ _ = []

{-
 - ( d1 ) ( a1 a2 ) = ( d1*a1 d1*a2 )
 - ( d2 )             ( d2*a1 d2*a2 )
 -}
weightDerivatives :: [Delta] -> [Activation] -> [Matrix Double]
weightDerivatives = zipWith (\d a -> d * transpose a)

-- activation - expected activation, a - y
costDerivative :: Activation -> Output -> Matrix Double
costDerivative = (-)

train :: (Activation, Output) -> Network -> Network
train (input, expected) ann = zipWith3 applyGradient dCdw dCdb ann
    where   dCdw        = weightDerivatives dCdb (input:as)
            dCdb        = deltas ws zs expected
            ws          = map fst ann
            (zs, as)    = feedForward ann input

applyGradient :: Matrix Double -> Matrix Double -> Layer -> Layer
applyGradient dCdw dCdb (w, b) = (w - dCdw, b - dCdb)
