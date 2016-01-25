module ML.MultipleRegression where

import Utils.Common

computeFeatureDerivative :: [Double] -> [Double] -> Double
computeFeatureDerivative errors feature = 2 * dotProductRow errors feature

