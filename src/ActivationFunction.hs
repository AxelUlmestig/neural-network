module ActivationFunction (
    activationFunc,
    activationFunc'
) where

activationFunc :: Double -> Double
activationFunc = sigmoid

activationFunc' :: Double -> Double
activationFunc' = sigmoid'

relu :: Double -> Double
relu = max 0

relu' :: Double -> Double
relu' x | x > 0 = 1
relu' _         = 0

sigmoid :: Double -> Double
sigmoid x = 1 / (1 + exp (negate x))

sigmoid' :: Double -> Double
sigmoid' x = sigmoid x * (1 - sigmoid x)
