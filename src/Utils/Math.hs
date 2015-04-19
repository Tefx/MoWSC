module Utils.Math (euclideanDis, ulamDis) where

import           Data.List.LCS.HuntSzymanski (lcs)
import qualified Data.Vector                 as Vec

euclideanDis::[Double]->[Double]->Double
euclideanDis xs ys = sqrt . sum . map (\x->x * x) $ zipWith (-) xs ys

ulamDis::Ord a=>Vec.Vector a->Vec.Vector a->Int
ulamDis a b = (Vec.length a -) . length $ lcs (Vec.toList a) (Vec.toList b)
