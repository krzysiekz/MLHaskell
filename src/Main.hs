module Main where

import System.Environment
import Files.CSVReader
import Utils.Common
import ML.SimpleLinearRegression

-- Simple linear regression test

main =
    do (trainingFileName:testFileName:_) <- getArgs
       training <- readFile trainingFileName
       case parseCSV training of
            Left e -> do
                         putStrLn "Error parsing input:"
                         print e
            Right r -> do
                          let inputFeature = convertColumnToNumeric (tail r) 5
                          let output = convertColumnToNumeric (tail r) 2
                          let (intercept, slope) = simpleLinearRegression inputFeature output
                          print $ show (intercept, slope)
                          print $ show $ getRegressionPredictions [2650] intercept slope
                          print $ show $ getResidualSumOfSquares inputFeature output intercept slope
                          print $ show $ inverseRegressionPredictions 800000 intercept slope
                          test <- readFile testFileName
                          case parseCSV test of
                                      Left eT -> do putStrLn "Error parsing input:"
                                                    print eT
                                      Right rT -> do
                                                    let testInputFeature = convertColumnToNumeric (tail rT) 5
                                                    let testOutput = convertColumnToNumeric (tail rT) 2
                                                    print $ show $ getResidualSumOfSquares testInputFeature testOutput intercept slope

