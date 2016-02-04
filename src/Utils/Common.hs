module Utils.Common where

import Text.Read

convertColumnToNumeric :: [[String]] -> Int -> [Double]
convertColumnToNumeric [] _ = []
convertColumnToNumeric (row@(x:xs):ys) index =
    let value = row!!index
    in case (readMaybe value :: Maybe Double) of
        Just converted -> converted : convertColumnToNumeric ys index
        Nothing -> error ("Not numeric value: " ++ value)

getColumn :: [[Double]] -> Int -> [Double]
getColumn [] _ = []
getColumn (row@(x:xs):ys) index =
    let value = row!!index
    in value : getColumn ys index

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

predictOutput :: [[Double]] -> [Double] -> [Double]
predictOutput = dotProduct

-- dotProduct :: [[Double]] -> [Double] -> [Double]
-- dotProduct _ [] = error "Empty array with weights"
-- dotProduct [] _ = []
-- dotProduct (row@(r:rs):xs) weights = dotProductRow row weights : dotProduct xs weights

-- dotProductRow :: [Double] -> [Double] -> Double
-- dotProductRow [] _ = 0
-- dotProductRow row@(x:xs) weights@(y:ys)
--     | length row == length weights = x*y + dotProductRow xs ys
--     | otherwise = error "Cannot multiply matrix, wrong sizes"


dotProduct :: [[Double]] -> [Double] -> [Double]
dotProduct _ [] = error "Empty array with weights"
dotProduct [] _ = []
dotProduct matrix weights = map (dotProductRow weights) matrix

dotProductRow :: [Double] -> [Double] -> Double
dotProductRow a b | length a == length b = sum (zipWith (*) a b)
                  | otherwise = error "Vector sizes must match"

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