module Main where

import System.Environment
import Files.CSVReader
import Utils.Common
import ML.SimpleLinearRegression
import ML.MultipleRegression

-- Multiple regression test

dataFileName = "resources/kc_house_data.csv"
trainingFileName = "resources/kc_house_train_data.csv"
testFileName = "resources/kc_house_test_data.csv"

main =
    do
       allDataRaw <- readFile dataFileName
       case parseCSV allDataRaw of
            Left err -> do
                         putStrLn "Error parsing input:"
                         print err
            Right allData -> do
--                     putStrLn ("Read " ++ show (length allData) ++ " lines")
--                     let (featuresMatrix, outputArray) = getMatrixData (tail allData) [5] 2
--                     print $ show $ head featuresMatrix
--                     print $ show $ head outputArray
--                     let weights = replicate 2 1
--                     let testPredictions = predictOutput featuresMatrix weights
--                     print $ head testPredictions
--                     print $ testPredictions !! 1
--                     let weights2 = replicate 2 0
--                     let testPredictions2 = predictOutput featuresMatrix weights2
--                     let errors = zipWith (-) testPredictions2 outputArray
--                     let feature = getColumn featuresMatrix 0
--                     let derivative = computeFeatureDerivative errors feature
--                     print derivative
                    trainingDataRaw <- readFile trainingFileName
                    case parseCSV trainingDataRaw of
                        Left err -> do
                                     putStrLn "Error parsing input:"
                                     print err
                        Right trainingData -> do
                               let (featuresMatrixTraining, outputArrayTraining) = getMatrixData (tail trainingData) [5] 2
                               let trainingInitialWeights = [-47000, 1]
                               let trainingStepSize = 7e-12
                               let trainingTolerance = 2.5e7
                               let calculatedTrainingWeights = regressionGradientDescent featuresMatrixTraining outputArrayTraining trainingInitialWeights trainingStepSize trainingTolerance
                               print calculatedTrainingWeights