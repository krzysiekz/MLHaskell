module Main where

import           Files.CSVReader
import           Utils.Common
import ML.NearestNeighborsRegression
import Data.List

-- Multiple regression test

dataFileName = "resources/kc_house_data_small.csv"
trainingFileName = "resources/kc_house_data_small_train.csv"
testFileName = "resources/kc_house_data_small_test.csv"

main =
    do
        trainingDataRaw <- readFile trainingFileName
        case parseCSV trainingDataRaw of
            Left err -> do
                putStrLn "Error parsing input:"
                print err
            Right trainingData -> do
                let (featuresMatrixTraining, outputArrayTraining) = getMatrixData (tail trainingData) [3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20] 2
                let (normalizedFeaturesMatrixTraining, normsTraining) = normalizeFeatures featuresMatrixTraining

                testDataRaw <- readFile testFileName
                case parseCSV testDataRaw of
                    Left err -> do
                        putStrLn "Error parsing input:"
                        print err
                    Right testData -> do
                        let (featuresMatrixTest, outputArrayTest) = getMatrixData (tail testData) [3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20] 2
                        let normalizedFeaturesMatrixTest =  map (\row -> zipWith (/) row normsTraining ) featuresMatrixTest
                        let distance = euclideanDistance (head normalizedFeaturesMatrixTest) (normalizedFeaturesMatrixTraining!!9)
                        print distance

                        let distancesAll = map (euclideanDistance (normalizedFeaturesMatrixTest!!2)) normalizedFeaturesMatrixTraining
                        let minIndex = elemIndex (minimum distancesAll) distancesAll
                        print minIndex
                        case minIndex of
                            Just index -> print $ outputArrayTraining!!index
                            Nothing -> print "No index found"

                        let distancesWithIndexes = zipWith (\a b -> (a,b)) distancesAll [0..]
                        let distancesWithIndexesSorted = increasedSort distancesWithIndexes
                        print $ take 4 distancesWithIndexesSorted

                        let averagePrediction = foldl (\sum index -> sum + outputArrayTraining!!index) 0 (map snd (take 4 distancesWithIndexesSorted)) / 4
                        print averagePrediction


increasedSort :: [(Double, Int)] -> [(Double, Int)]
increasedSort = sortBy (\(x1, y1) (x2, y2) ->  x1 `compare` x2)