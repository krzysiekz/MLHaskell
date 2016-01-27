module ML.MultipleRegression where

import Utils.Common

computeFeatureDerivative :: [Double] -> [Double] -> Double
computeFeatureDerivative errors feature = 2 * dotProductRow errors feature

regressionGradientDescent :: [[Double]] -> [Double] -> [Double] -> Double -> Double -> [Double]
regressionGradientDescent [] _ _ _ _ = error "Empty feature matrix"
regressionGradientDescent _ [] _ _ _ = error "Empty output array"
regressionGradientDescent _ _ [] _ _ = error "Empty weights array"
regressionGradientDescent featureMatrix output initialWeights stepSize tolerance
    | gradientMagnitude < tolerance = updatedWeights
    | otherwise = regressionGradientDescent featureMatrix output updatedWeights stepSize tolerance
    where errors = zipWith (-) (predictOutput featureMatrix initialWeights) output
          (updatedWeights, gradientSumSquares) = updateWeights featureMatrix errors initialWeights [] 0 stepSize 0
          gradientMagnitude = sqrt gradientSumSquares

updateWeights :: [[Double]] -> [Double] -> [Double] -> [Double] -> Double -> Double -> Int -> ([Double], Double)
updateWeights _ _ [] updatedWeights gradientSumSquares _ _= (updatedWeights, gradientSumSquares)
updateWeights featureMatrix errors weights@(x:xs) updatedWeights gradientSumSquares stepSize index =
    updateWeights featureMatrix errors xs (updatedWeights ++ [newWeightValue]) newGradientSumSquares stepSize (index+1)
    where derivative = computeFeatureDerivative errors $ getColumn featureMatrix index
          newGradientSumSquares = gradientSumSquares + (derivative * derivative)
          newWeightValue = x - (derivative * stepSize)
