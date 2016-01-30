module Main where

import System.Environment
import qualified Files.CSVReader as CSVReader
import Utils.Common
import ML.RidgeRegression
import ML.MultipleRegression
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import qualified Data.List as List

dataFileName = "resources/kc_house_data.csv"
trainingFileName = "resources/kc_house_train_data.csv"
testFileName = "resources/kc_house_test_data.csv"

main =
    do
       allDataRaw <- readFile dataFileName
       case CSVReader.parseCSV allDataRaw of
            Left err -> do
                         putStrLn "Error parsing input:"
                         print err
            Right allData -> do
                    putStrLn ("Read " ++ show (length allData) ++ " lines of all data")
                    let (featuresMatrix, outputArray) = getMatrixData (tail allData) [5] 2
                    let weights = [1, 10]
                    let testPredictions = predictOutput featuresMatrix weights
                    let errors = zipWith (-) testPredictions outputArray
                    let featureDerivative = featureDerivativeRidge errors (getColumn featuresMatrix 1) (weights!!1) 1 False
                    print featureDerivative

                    let featureDerivative2 = featureDerivativeRidge errors (getColumn featuresMatrix 0) (head weights) 1 True
                    print featureDerivative2

                    trainingDataRaw <- readFile trainingFileName
                    case CSVReader.parseCSV trainingDataRaw of
                        Left err -> do
                            putStrLn "Error parsing input:"
                            print err
                        Right trainingData -> do
                            let (featuresMatrixTraining, outputArrayTraining) = getMatrixData (tail trainingData) [5] 2
                            let trainingInitialWeights = [0, 0]
                            let trainingStepSize = 1e-12
                            let simpleWeights0Penalty = ridgeRegressionGradientDescent featuresMatrixTraining outputArrayTraining trainingInitialWeights trainingStepSize 0 100
                            print simpleWeights0Penalty

                            let simpleWeightsHighPenalty = ridgeRegressionGradientDescent featuresMatrixTraining outputArrayTraining trainingInitialWeights trainingStepSize 1e11 100
                            print simpleWeightsHighPenalty

                            toFile def "ridgeRegression.png" $ do
                                layout_title .= "Training data"
                                layout_y_axis . laxis_title .= "Price"
                                layout_x_axis . laxis_title .= "Sqft living"
                                setColors [opaque blue, opaque red, opaque green]
                                plot (points "raw data" (increasedSort $ zipWith (\ a b -> (a,b)) (getColumn featuresMatrixTraining 1) outputArrayTraining))
                                plot (line "predicted no l2" [increasedSort $ zipWith (\ a b -> (a,b)) (getColumn featuresMatrixTraining 1) (predictOutput featuresMatrixTraining simpleWeights0Penalty)])
                                plot (line "predicted high l2" [increasedSort $ zipWith (\ a b -> (a,b)) (getColumn featuresMatrixTraining 1) (predictOutput featuresMatrixTraining simpleWeightsHighPenalty)])

                            let (featuresMatrixTrainingMulti, outputArrayTrainingMulti) = getMatrixData (tail trainingData) [5,19] 2
                            let trainingInitialWeightsMulti = [0, 0, 0]
                            let maxIterations = 1000
                            let multiWeights0Penalty = ridgeRegressionGradientDescent featuresMatrixTrainingMulti outputArrayTrainingMulti trainingInitialWeightsMulti trainingStepSize 0 maxIterations
                            print multiWeights0Penalty

                            let multiWeightsHighPenalty = ridgeRegressionGradientDescent featuresMatrixTrainingMulti outputArrayTrainingMulti trainingInitialWeightsMulti trainingStepSize 1e11 maxIterations
                            print multiWeightsHighPenalty

increasedSort :: [(Double, Double)] -> [(Double, Double)]
increasedSort = List.sortBy (\(x1, y1) (x2, y2) ->  x1 `compare` x2)