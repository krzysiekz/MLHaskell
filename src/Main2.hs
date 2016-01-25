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
       training <- readFile dataFileName
       case parseCSV training of
            Left err -> do
                         putStrLn "Error parsing input:"
                         print err
            Right trainingData -> do
                    putStrLn ("Read " ++ show (length trainingData) ++ " lines")
                    let (featuresMatrix, outputArray) = getMatrixData (tail trainingData) [5] 2
                    print $ show $ head featuresMatrix
                    print $ show $ head outputArray
                    let weights = replicate 2 1
                    let testPredictions = predictOutput featuresMatrix weights
                    print $ head testPredictions
                    print $ testPredictions !! 1
                    let weights2 = replicate 2 0
                    let testPredictions2 = predictOutput featuresMatrix weights2
                    let errors = zipWith (-) testPredictions2 outputArray
                    let feature = getColumn featuresMatrix 0
                    let derivative = computeFeatureDerivative errors feature
                    print derivative