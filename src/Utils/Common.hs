module Utils.Common where

import Text.Read

convertColumnToNumeric :: [[String]] -> Int -> [Double]
convertColumnToNumeric [] _ = []
convertColumnToNumeric (row@(x:xs):ys) index =
    let value = row!!index
    in case (readMaybe value :: Maybe Double) of
        Just converted -> converted : convertColumnToNumeric ys index
        Nothing -> error ("Not numeric value: " ++ value)
