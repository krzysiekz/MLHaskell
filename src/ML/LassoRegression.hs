module ML.LassoRegression where

import Utils.Common

normalizeFeatures :: [[Double]] -> ([[Double]],[Double])
normalizeFeatures [] = error "Empty features matrix"
normalizeFeatures featuresMatrix@(x:xs) = (map (\row -> zipWith (/) row sndNorm) featuresMatrix,sndNorm)
    where sndNorm = secondNorm featuresMatrix (length x)

secondNorm :: [[Double]] -> Int -> [Double]
secondNorm [] _ = error "Empty features matrix"
secondNorm _ 0 = []
secondNorm featuresMatrix index = secondNorm featuresMatrix (index-1) ++ [columnNorm]
    where columnNorm = calculateColumnNorm (getColumn featuresMatrix (index-1))

calculateColumnNorm :: [Double] -> Double
calculateColumnNorm [] = error "Empty column"
calculateColumnNorm column = sqrt (foldl (\acc x -> acc + (x**2)) 0 column)
