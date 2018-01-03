import Data.Matrix hiding (zero)
import System.Random

import Network
import Backpropagation

zerozero = fromList 2 1 [0, 0]
zeroone = fromList 2 1 [0, 1]
onezero = fromList 2 1 [1, 0]
oneone = fromList 2 1 [1, 1]

zero = fromList 1 1 [0]
one = fromList 1 1 [1]

trainingData :: [(Matrix Double, Matrix Double)]
trainingData = cycle [(zerozero, zero), (zeroone, one), (onezero, one), (oneone, zero)]

applyTrainingData :: Network -> [(Matrix Double, Matrix Double)] -> Network
applyTrainingData = foldr train

xor :: Network
xor = (fromList 2 2 [20, 20, -20, -20], fromList 2 1 [-10, 30]) : (fromList 1 2 [20, 20], fromList 1 1 [-30]) : []

main :: IO ()
main = do
    g <- newStdGen
    let ann = initializeNetwork g [2,2,1]
    putStrLn . show $ query ann onezero
    let ann' = applyTrainingData ann (take 20000 trainingData)
    putStrLn . ("0 0: " ++) . show $ query ann' zerozero
    putStrLn . ("0 1: " ++) . show $ query ann' onezero
    putStrLn . ("1 0: " ++) . show $ query ann' zeroone
    putStrLn . ("1 1: " ++) . show $ query ann' oneone
    putStrLn "trained ann: "
    putStrLn . show $ ann'
