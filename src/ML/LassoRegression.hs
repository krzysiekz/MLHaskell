module ML.LassoRegression where

import Utils.Common

lassoCyclicalCoordinateDescent :: [[Double]] -> [Double] -> [Double] -> Double -> Double -> [Double]
lassoCyclicalCoordinateDescent [] _ _ _ _ = error "Empty features matrix"
lassoCyclicalCoordinateDescent _ [] _ _ _ = error "Empty output array"
lassoCyclicalCoordinateDescent _ _ [] _ _ = error "Empty initial weights"
lassoCyclicalCoordinateDescent featuresMatrix output initialWeights l1Penalty tolerance
    | maxChange <= tolerance = updatedWeights
    | otherwise = lassoCyclicalCoordinateDescent featuresMatrix output updatedWeights l1Penalty tolerance
    where (updatedWeights, maxChange) = lassoUpdateWeights featuresMatrix output initialWeights [] l1Penalty 0 0
          lassoUpdateWeights :: [[Double]] -> [Double] -> [Double] -> [Double] -> Double -> Double -> Int -> ([Double], Double)
          lassoUpdateWeights _ _ [] updatedWeights _ maxChange _= (updatedWeights, maxChange)
          lassoUpdateWeights featuresMatrix output weights@(x:xs) updatedWeights l1Penalty maxChange index =
              lassoUpdateWeights featuresMatrix output xs (updatedWeights ++ [newWeightValue]) l1Penalty newMaxChange (index + 1)
              where newWeightValue = lassoCoordinateDescentStep index featuresMatrix output (updatedWeights ++ weights) l1Penalty
                    change = abs (x - newWeightValue)
                    newMaxChange = if change > maxChange then change else maxChange
                    lassoCoordinateDescentStep :: Int -> [[Double]] -> [Double] -> [Double] -> Double -> Double
                    lassoCoordinateDescentStep index featuresMatrix output weights l1Penalty
                        | index == 0 = ro
                        | ro < ((-l1Penalty)/2) = ro + (l1Penalty/2)
                        | ro > (l1Penalty/2) = ro - (l1Penalty/2)
                        | otherwise = 0
                        where prediction = predictOutput featuresMatrix weights
                              singleFeature = getColumn featuresMatrix index
                              weight = weights!!index
                              ro = sum $ zipWith (*) singleFeature $ zipWith (+) (zipWith (-) output prediction) (map (*weight) singleFeature)