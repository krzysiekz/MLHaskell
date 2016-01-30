module ML.RidgeRegression where

import Utils.Common

featureDerivativeRidge :: [Double] -> [Double] -> Double ->Double -> Bool -> Double
featureDerivativeRidge errors feature weight l2Penalty featureIsConstant =
    if featureIsConstant then product else product + 2 * l2Penalty * weight
    where product = 2 * dotProductRow errors feature

ridgeRegressionGradientDescent :: [[Double]] -> [Double] -> [Double] -> Double -> Double -> Int -> [Double]
ridgeRegressionGradientDescent [] _ _ _ _ _ = error "Empty feature matrix"
ridgeRegressionGradientDescent _ [] _ _ _ _ = error "Empty output array"
ridgeRegressionGradientDescent _ _ [] _ _ _ = error "Empty weights array"
ridgeRegressionGradientDescent featureMatrix output initialWeights stepSize l2Penalty maxIterations
    | maxIterations <= 0 = updatedWeights
    | otherwise = ridgeRegressionGradientDescent featureMatrix output updatedWeights stepSize l2Penalty (maxIterations -1)
    where errors = zipWith (-) (predictOutput featureMatrix initialWeights) output
          updatedWeights = ridgeUpdateWeights featureMatrix errors initialWeights [] l2Penalty stepSize 0

ridgeUpdateWeights :: [[Double]] -> [Double] -> [Double] -> [Double] -> Double -> Double -> Int -> [Double]
ridgeUpdateWeights _ _ [] updatedWeights _ _ _= updatedWeights
ridgeUpdateWeights featureMatrix errors weights@(x:xs) updatedWeights l2Penalty stepSize index =
    ridgeUpdateWeights featureMatrix errors xs (updatedWeights ++ [newWeightValue]) l2Penalty stepSize (index+1)
    where derivative = featureDerivativeRidge errors (getColumn featureMatrix index) x l2Penalty (index==0)
          newWeightValue = x - (derivative * stepSize)

