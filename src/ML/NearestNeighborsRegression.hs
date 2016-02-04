module ML.NearestNeighborsRegression where

euclideanDistance :: [Double] -> [Double] -> Double
euclideanDistance firstFeatures secondFeatures
    | length firstFeatures == length secondFeatures = sqrt $ sum $ map (**2) $ zipWith (-) secondFeatures firstFeatures
    | otherwise = error "Cannot compute distance. Different sizes"
