module Main where

import System.Environment
import Files.CSVReader
import Utils.Common
import ML.SimpleLinearRegression

-- Multiple regression test

trainingFileName = "resources/kc_house_train_data.csv"
testFileName = "resources/kc_house_test_data.csv"

main =
    do
       training <- readFile trainingFileName
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