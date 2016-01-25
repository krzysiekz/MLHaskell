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
            Left e -> do
                         putStrLn "Error parsing input:"
                         print e
            Right r -> putStrLn "TBD"