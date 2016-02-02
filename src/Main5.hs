module Main where

import qualified Files.CSVReader as CSVReader
import Utils.Common
import ML.LassoRegression

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
                    putStrLn ("Lasso. Read " ++ show (length allData) ++ " lines of all data")
                    let (featuresMatrix, outputArray) = getMatrixData (tail allData) [5,3] 2
                    let (normalizedFeaturesMatrix, norms) = normalizeFeatures featuresMatrix
                    let initialWeights = replicate 3 0
                    let l1Penalty = 1e7
                    let tolerance = 1.0
                    let weights = lassoCyclicalCoordinateDescent normalizedFeaturesMatrix outputArray initialWeights l1Penalty tolerance
                    print weights

                    trainingDataRaw <- readFile trainingFileName
                    case CSVReader.parseCSV trainingDataRaw of
                        Left err -> do
                            putStrLn "Error parsing input:"
                            print err
                        Right trainingData -> do
                            let (featuresMatrixTraining, outputArrayTraining) = getMatrixData (tail trainingData) [3,4,5,6,7,8,9,10,11,12,13,14] 2
                            let (normalizedFeaturesMatrixTraining, normsTraining) = normalizeFeatures featuresMatrixTraining
                            let initialWeightsTraining = replicate 13 0
                            let weightsTraining = lassoCyclicalCoordinateDescent normalizedFeaturesMatrixTraining outputArrayTraining initialWeightsTraining l1Penalty tolerance
                            print weightsTraining

                            let l1Penalty2 = 1e8
                            let weightsTraining2 = lassoCyclicalCoordinateDescent normalizedFeaturesMatrixTraining outputArrayTraining initialWeightsTraining l1Penalty2 tolerance
                            print weightsTraining2

                            let l1Penalty3 = 1e4
                            let tolerance2 = 5e5
                            let weightsTraining3 = lassoCyclicalCoordinateDescent normalizedFeaturesMatrixTraining outputArrayTraining initialWeightsTraining l1Penalty3 tolerance2
                            print weightsTraining3
