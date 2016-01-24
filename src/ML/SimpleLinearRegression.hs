module ML.SimpleLinearRegression where

simpleLinearRegression :: [Double] -> [Double] -> (Double, Double)
simpleLinearRegression inputFeature output = (mean output - (slope * mean inputFeature), slope)
    where slope = (productSum inputFeature output - (sum output*sum inputFeature) / fromIntegral (length inputFeature))
                / (productSum inputFeature inputFeature - (sum inputFeature * sum inputFeature) / fromIntegral (length inputFeature))

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

productSum :: [Double] -> [Double] -> Double
productSum xs ys = sum (zipWith (*) xs ys)

getRegressionPredictions:: [Double] -> Double -> Double -> [Double]
getRegressionPredictions inputFeature intercept slope = map ((intercept +).(slope *)) inputFeature

getResidualSumOfSquares :: [Double] -> [Double] -> Double -> Double -> Double
getResidualSumOfSquares inputFeature output intercept slope =
    let predictions = getRegressionPredictions inputFeature intercept slope
    in sum (map (\x -> x*x)(zipWith (-) output predictions))

inverseRegressionPredictions:: Double -> Double -> Double -> Double
inverseRegressionPredictions output intercept slope =  (output - intercept) / slope

