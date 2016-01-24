module Utils.Common where

import Text.Read

convertColumnToNumeric :: [[String]] -> Int -> [Double]
convertColumnToNumeric [] _ = []
convertColumnToNumeric (row@(x:xs):ys) index =
    let value = row!!index
    in case (readMaybe value :: Maybe Double) of
        Just converted -> converted : convertColumnToNumeric ys index
        Nothing -> error ("Not numeric value: " ++ value)

getMatrixData :: [[String]] -> [Int] -> Int -> ([[Double]], [Double])
getMatrixData [] _ _ = error "Input data is empty"
getMatrixData _ [] _ = error "Features indexes are empty"
getMatrixData inputFeatures featuresIndexes outputIndex =
    (getFeaturesMatrix inputFeatures featuresIndexes, convertColumnToNumeric inputFeatures outputIndex)

getFeaturesMatrix :: [[String]] -> [Int] -> [[Double]]
getFeaturesMatrix [] _ = []
getFeaturesMatrix (row@(r:rs):zs) featuresIndexes =
    (1 : getFeaturesRow row featuresIndexes) : getFeaturesMatrix zs featuresIndexes

getFeaturesRow :: [String] -> [Int] -> [Double]
getFeaturesRow _ [] = []
getFeaturesRow row (x:xs) =
    let value = row!!x
    in case (readMaybe value :: Maybe Double) of
        Just converted -> converted : getFeaturesRow row xs
        Nothing -> error ("Not numeric value: " ++ value)
