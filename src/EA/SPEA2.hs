{-# LANGUAGE TupleSections #-}

module EA.SPEA2 (WithSPEA2Fit, assignSPEA2Fit, spea2Select) where

import           EA            (EnvSelector)
import           EA.Fitness    (FitnessAssigner, WithFitness (..))
import           MOP           (Normaliser, Objectives, initNorm, norm, (<<<))
import           Utils         (With (..))
import           Utils.Math    (euclideanDis)

import           Data.Function (on)
import           Data.List     (sort, sortBy)
import qualified Data.Vector   as Vec

type WithSPEA2Fit = WithFitness SPEA2Fit

assignSPEA2Fit::(Objectives o)=>FitnessAssigner SPEA2Fit c o
assignSPEA2Fit is = let nz = initNorm is
                        rs = Vec.map (fromIntegral) . calRaw . calS $ is
                        k = round . sqrt . fromIntegral $ Vec.length is
                        ds = Vec.map ((1/) . (+2) . (!!k)) $ calDs nz is
                    in Vec.zipWith (\x y->WithFit x $ SPEA2Fit y) is $
                       Vec.zipWith (+) rs ds

spea2Select::(Objectives o)=>EnvSelector c o
spea2Select pop0 pop1 = let pop = (Vec.++) pop0 pop1
                            n = Vec.length pop0
                            popD = assignSPEA2Fit pop
                            (p_, p) = Vec.unstablePartition nonDominated popD
                        in case compare (Vec.length p_) n of
                            EQ -> Vec.map runWithFit p_
                            LT -> Vec.map runWithFit .
                                  (Vec.++) p_ .
                                  Vec.fromList. take (n - Vec.length p_) .
                                  sortBy (compare `on` fitness) . Vec.toList $ p
                            GT -> trunc (initNorm pop) n $ Vec.map runWithFit p_


-- Non-export --

newtype SPEA2Fit = SPEA2Fit {_fit :: Double} deriving (Eq)

instance Ord SPEA2Fit where
  compare = compare `on` _fit

nonDominated::WithSPEA2Fit c o->Bool
nonDominated (WithFit _ (SPEA2Fit i)) = i < 1


-- assign Fitness --

calS::(Objectives o)=>Vec.Vector (With c o)->Vec.Vector (With c o, Int)
calS pop = Vec.map (\x->(x,) . Vec.length $ Vec.filter ((<<<) (elem1 x) . elem1) pop) pop

calRaw::(Objectives o)=>Vec.Vector (With c o, Int)->Vec.Vector Int
calRaw is = Vec.map (\(x, _)->Vec.sum . Vec.map snd $
                              Vec.filter ((<<<) (elem1 x) . elem1 . fst) is) is

calDs::(Objectives o)=>Normaliser->Vec.Vector (With c o)->Vec.Vector [Double]
calDs nz is = Vec.map (\i->sort . Vec.toList $ Vec.map (eucInd i) is) is
  where eucInd = euclideanDis `on` (norm nz)

-- select --

compareByKD::(a, [Double])->(a, [Double])->Ordering
compareByKD (_, []) (_, []) = EQ
compareByKD (a, x:xs) (b, y:ys)
  | x == y = compareByKD (a, xs) (b, ys)
  | otherwise = compare x y

trunc::(Objectives o)=>Normaliser->Int->Vec.Vector (With c o)->Vec.Vector (With c o)
trunc nz n pop
  | n == Vec.length pop = pop
  | otherwise = let pd = Vec.zipWith (,) pop $ calDs nz pop
                    r = Vec.minIndexBy compareByKD pd
                in trunc nz n $ Vec.ifilter (\i _->i /= r) pop
