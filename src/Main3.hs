module Main where

import System.Environment
import qualified Files.CSVReader as CSVReader
import Utils.Common
import ML.SimpleLinearRegression
import ML.MultipleRegression
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.List as List

dataFileName = "resources/kc_house_data.csv"
trainingFileName = "resources/kc_house_train_data.csv"
testFileName = "resources/kc_house_test_data.csv"

main =
    do
       trainingDataRaw <- readFile trainingFileName
       case CSVReader.parseCSV trainingDataRaw of
        Left err -> do
            putStrLn "Error parsing input:"
            print err
        Right trainingData -> do
            let sqrtLiving = convertColumnToNumeric (tail trainingData) 5
            let price = convertColumnToNumeric (tail trainingData) 2
            let sqrtLiving1 = createPolynomialMatrix sqrtLiving 1
            let sqrtLiving1InitialWeights = [-47000, 1]
            let sqrtLiving1StepSize = 7e-12
            let sqrtLiving1Tolerance = 2.5e7
            let sqrtLiving1Weights = regressionGradientDescent sqrtLiving1 price sqrtLiving1InitialWeights sqrtLiving1StepSize sqrtLiving1Tolerance
            print sqrtLiving1Weights

            toFile def "sqrtLiving1.png" $ do
                layout_title .= "Training data"
                layout_y_axis . laxis_title .= "Price"
                layout_x_axis . laxis_title .= "Sqft living"
                setColors [opaque blue, opaque red]
                plot (points "raw data" (zipWith (\ a b -> (a,b)) (getColumn sqrtLiving1 1) price))
                plot (line "predicted" [signal (getColumn sqrtLiving1 1) (\x ->  head sqrtLiving1Weights + (x * sqrtLiving1Weights!!1))])

--             let sqrtLiving2 = createPolynomialMatrix sqrtLiving 2
--             let sqrtLiving2InitialWeights = [-100000, 1, 1]
--             let sqrtLiving2StepSize = 1e-17
--             let sqrtLiving2Tolerance = 0.2e19
--             let sqrtLiving2Weights = regressionGradientDescent sqrtLiving2 price sqrtLiving2InitialWeights sqrtLiving2StepSize sqrtLiving2Tolerance
--             print sqrtLiving2Weights
--
--             let sqrtLiving2Predicted = signal (getColumn sqrtLiving2 1) (\x ->  head sqrtLiving2Weights + (x * sqrtLiving2Weights!!1) + ((x**2) * sqrtLiving2Weights!!2))
--             let sqrtLiving2PredictedSorted = increasedSort sqrtLiving2Predicted
--             toFile def "sqrtLiving2.png" $ do
--                 layout_title .= "Training data"
--                 layout_y_axis . laxis_title .= "Price"
--                 layout_x_axis . laxis_title .= "Sqft living"
--                 setColors [opaque blue, opaque red]
--                 plot (points "raw data" (zipWith (\ a b -> (a,b)) (getColumn sqrtLiving2 1) price))
--                 plot (line "predicted" [sqrtLiving2PredictedSorted])

createPolynomialMatrix :: [Double] -> Double -> [[Double]]
createPolynomialMatrix [] _ = []
createPolynomialMatrix (x:xs) power = foldr (\pow acc -> x**pow : acc) [] [0..power] : createPolynomialMatrix xs power

signal :: [a] -> (a -> a1) -> [(a, a1)]
signal xs f = [ (x,f x) | x <- xs ]

increasedSort :: [(Double, Double)] -> [(Double, Double)]
increasedSort = List.sortBy (\(x, _) (y, _) ->  x `compare` y)